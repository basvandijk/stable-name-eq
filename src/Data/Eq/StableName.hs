{-# language CPP #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TemplateHaskell #-}

module Data.Eq.StableName
    ( StableNameEq, ($==), (==$)

    , StableNameEq1(..), StableNameEq2(..)

      -- * Deriving instances using Template Haskell
    , deriveStableNameEq
    , deriveStableNameEq1
    , deriveStableNameEq2
    ) where

import System.Mem.StableName
import Control.Applicative ((<|>))
import Control.Monad (liftM2, unless, when)
import Data.Foldable (foldr')
import Data.List (foldl', genericLength, intercalate, partition, union)
import Data.List.NonEmpty ((<|), NonEmpty((:|)))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Monoid as Monoid
import Data.Set (Set)
#if MIN_VERSION_template_haskell(2,8,0)
import Language.Haskell.TH hiding (Arity)
#else
import Language.Haskell.TH
#endif
import Language.Haskell.TH.Datatype
#if MIN_VERSION_template_haskell(2,7,0) && !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Lib (starK)
#endif
import qualified Data.Foldable as F (all)
import qualified Data.List.NonEmpty as NE (length, reverse)
import qualified Data.Map as M (fromList, keys, lookup , singleton, size)
import qualified Data.Semigroup as Semigroup (Option(..))
import qualified Data.Set as Set (empty, insert, member)


--------------------------------------------------------------------------------
-- StableName Equality
--------------------------------------------------------------------------------

($==) :: (StableNameEq a) => a -> a -> IO Bool
($==) !x !y = do
    sx <- makeStableName x
    sy <- makeStableName y
    if sx == sy
      then pure True
      else x ==$ y

class StableNameEq a where
    (==$) :: a -> a -> IO Bool

-- | Fall-back instance which uses '=='.
instance {-# OVERLAPPABLE #-} (Eq a) => StableNameEq a where
    x ==$ y = pure $ x == y

class StableNameEq1 f where
    liftSNEq1
        :: (a -> b -> IO Bool)
        -> f a -> f b -> IO Bool

class StableNameEq2 f where
    liftSNEq2
        :: (a -> b -> IO Bool)
        -> (c -> d -> IO Bool)
        -> f a c -> f b d -> IO Bool


--------------------------------------------------------------------------------
-- Template Haskell
--------------------------------------------------------------------------------

-- Most of this code is copied from "aeson" Data.Aeson.TH.

deriveStableNameEq  = deriveStableNameEqCommon Arity0
deriveStableNameEq1 = deriveStableNameEqCommon Arity1
deriveStableNameEq2 = deriveStableNameEqCommon Arity2

deriveStableNameEqCommon :: Arity -> Name -> Q [Dec]
deriveStableNameEqCommon arity name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext = ctxt
                 , datatypeName    = parentName
                 , datatypeVars    = vars
                 , datatypeVariant = variant
                 , datatypeCons    = cons
                 } -> do
      (instanceCxt, instanceType)
        <- buildTypeInstance parentName arity ctxt vars variant
      (:[]) <$> instanceD (return instanceCxt)
                          (return instanceType)
                          (error "TODO")

-- For the given Types, generate an instance context and head.
buildTypeInstance :: Name
                  -- ^ The type constructor or data family name
                  -> Arity
                  -> Cxt
                  -- ^ The datatype context
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> DatatypeVariant
                  -- ^ Are we dealing with a data family instance or not
                  -> Q (Cxt, Type)
buildTypeInstance tyConName arity dataCxt varTysOrig variant = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - fromEnum arity

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || elem NotKindStar droppedStarKindStati) $
      derivingKindError arity tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = freeVariables droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError arity tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint arity) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (droppedKindVarNames `union` kvNames'))
            $ take remainingLength varTysOrig

        isDataFamily :: Bool
        isDataFamily = case variant of
                         Datatype        -> False
                         Newtype         -> False
                         DataInstance    -> True
                         NewtypeInstance -> True

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ eqClassName arity)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: Arity -> Type -> (Maybe Pred, [Name])
deriveConstraint arity t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t   = (Just (applyCon (eqClassName Arity0) tName), [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns | arity >= Arity1
              -> (Just (applyCon (eqClassName Arity1) tName), ns)
      _ -> case hasKindVarChain 2 t of
           Just ns | arity == Arity2
                   -> (Just (applyCon (eqClassName Arity2) tName), ns)
           _ -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

{-
Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is possible to put explicit kind signatures into the derived instances, e.g.,
  instance C a => C (Data (f :: * -> *)) where ...
But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since Template Haskell doesn't always
have the best track record with reifying kind signatures), then GHC will flat-out
reject the instance, which is quite unfortunate.
Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.
Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,
  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)
If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the mk- functions.
Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:
1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had
     data Data (a :: k) (b :: k)
   Then you'd want to derived instance to be:
     instance C (Data (a :: *))
   Not:
     instance C (Data (a :: k))
2. We naïvely come up with instance constraints using the following criteria:
   (i)   If there's a type parameter n of kind *, generate a ToJSON n/FromJSON n
         constraint.
   (ii)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
         variables), then generate a ToJSON1 n/FromJSON1 n constraint, and if
         k1/k2 are kind variables, then substitute k1/k2 with * elsewhere in the
         types. We must consider the case where they are kind variables because
         you might have a scenario like this:
           newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
             = Compose (f (g a))
         Which would have a derived ToJSON1 instance of:
           instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Compose f g) where ...
   (iii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
         * or kind variables), then generate a ToJSON2 n/FromJSON2 n constraint
         and perform kind substitution as in the other cases.
-}


{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When deriving ToJSON2, there is a tricky corner case to consider:
  data Both a b where
    BothCon :: x -> x -> Both x x
Which encoding functions should be applied to which arguments of BothCon?
We have a choice, since both the function of type (a -> Value) and of type
(b -> Value) can be applied to either argument. In such a scenario, the
second encoding function takes precedence over the first encoding function, so the
derived ToJSON2 instance would be something like:
  instance ToJSON2 Both where
    liftToJSON2 tj1 tj2 p (BothCon x1 x2) = Array $ create $ do
      mv <- unsafeNew 2
      unsafeWrite mv 0 (tj1 x1)
      unsafeWrite mv 1 (tj2 x2)
      return mv
This is not an arbitrary choice, as this definition ensures that
liftToJSON2 toJSON = liftToJSON for a derived ToJSON1 instance for
Both.
-}

-- A mapping of type variable Names to their encoding/decoding function Names.
-- For example, in a ToJSON2 declaration, a TyVarMap might look like
--
-- { a ~> (tj1, tjl1)
-- , b ~> (tj2, tjl2) }
--
-- where a and b are the last two type variables of the datatype, tj1 and tjl1 are
-- the function arguments of types (a -> Value) and ([a] -> Value), and tj2 and tjl2
-- are the function arguments of types (b -> Value) and ([b] -> Value).
type TyVarMap = Map Name (Name, Name)

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
#else
isStarOrVar StarK  = True
#endif
isStarOrVar _      = False

-- Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix len = mapM newName [prefix ++ show n | n <- [1..len]]

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (NE.length uk - 1 == kindArrows) && F.all isStarOrVar uk
        then Just (concatMap freeVariables uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToNameMaybe :: Type -> Maybe Name
varTToNameMaybe (VarT n)   = Just n
varTToNameMaybe (SigT t _) = varTToNameMaybe t
varTToNameMaybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToNameMaybe

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Is the given type a type family constructor (and not a data family constructor)?
isTyFamily :: Type -> Q Bool
isTyFamily (ConT n) = do
    info <- reify n
    return $ case info of
#if MIN_VERSION_template_haskell(2,11,0)
         FamilyI OpenTypeFamilyD{} _       -> True
#else
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#endif
#if MIN_VERSION_template_haskell(2,9,0)
         FamilyI ClosedTypeFamilyD{} _     -> True
#endif
         _ -> False
isTyFamily _ = return False

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2)    = t2 <| go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = t :| []

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, NonEmpty Type)
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1 <| tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], t :| [])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> NonEmpty Kind
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = snd . uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1 <| uncurryKind k2
uncurryKind k              = k :| []
#endif

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k 0 = k
#if MIN_VERSION_template_haskell(2,8,0)
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)
#else
    go k !n = go (ArrowK StarK k) (n - 1)
#endif

applyCon :: Name -> Name -> Pred
applyCon con t =
#if MIN_VERSION_template_haskell(2,10,0)
          AppT (ConT con) (VarT t)
#else
          ClassP con [VarT t]
#endif

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

applySubstitutionKind :: Map Name Kind -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
applySubstitutionKind = applySubstitution
#else
applySubstitutionKind _ t = t
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (M.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (`substNameWithKind` starK) t ns

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: Arity -> Name -> Q a
derivingKindError arity tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind "
  . showString (pprint . createKindChain $ (fromEnum arity :: Int))
  $ ""
  where
    className :: String
    className = nameBase $ eqClassName arity

eqClassName :: Arity -> Name
eqClassName Arity0 = ''StableNameEq
eqClassName Arity1 = ''StableNameEq1
eqClassName Arity2 = ''StableNameEq2

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString (nameBase dataName)
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of the arity of the ToJSON/FromJSON typeclass being derived.
data Arity = Arity0 | Arity1 | Arity2
  deriving (Enum, Eq, Ord)

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t = case t of
    _ | hasKindStar t -> KindStar
#if MIN_VERSION_template_haskell(2,8,0)
    SigT _ (VarT k) -> IsKindVar k
#endif
    _ -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName
