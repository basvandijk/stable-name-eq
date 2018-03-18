{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Eq.StableName.TH
    ( -- * 'StableNameEq'
      deriveStableNameEq
    , makeStableNameEq
    , makeNotStableNameEq

      -- * 'StableNameEq1'
    , deriveStableNameEq1
    , makeLiftStableNameEq
    , makeStableNameEq1

      -- * 'StableNameEq2'
    , deriveStableNameEq2
    , makeLiftStableNameEq2
    , makeStableNameEq2
    ) where

import Data.Eq.StableName.Class

import           Control.Applicative (liftA2)
import           Data.Deriving.Internal
import           Data.List (foldl1', partition)
import qualified Data.Map as Map

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------
-- Template Haskell
--------------------------------------------------------------------------------

-- | Generates an 'StableNameEq' instance declaration for the given data type or data
-- family instance.
deriveStableNameEq :: Name -> Q [Dec]
deriveStableNameEq = deriveStableNameEqClass StableNameEq

-- | Generates a lambda expression which behaves like '(==)' (without
-- requiring an 'StableNameEq' instance).
makeStableNameEq :: Name -> Q Exp
makeStableNameEq = makeStableNameEqClass StableNameEq

-- | Generates a lambda expression which behaves like '(/=)' (without
-- requiring an 'StableNameEq' instance).
makeNotStableNameEq :: Name -> Q Exp
makeNotStableNameEq name = do
    x1 <- newName "x1"
    x2 <- newName "x2"
    lamE [varP x1, varP x2] $ varE notValName `appE`
        (makeStableNameEq name `appE` varE x1 `appE` varE x2)

-- | Generates an 'StableNameEq1' instance declaration for the given data type or data
-- family instance.
deriveStableNameEq1 :: Name -> Q [Dec]
deriveStableNameEq1 = deriveStableNameEqClass StableNameEq1

-- | Generates a lambda expression which behaves like 'liftStableNameEq' (without
-- requiring an 'StableNameEq1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftStableNameEq :: Name -> Q Exp
makeLiftStableNameEq = makeStableNameEqClass StableNameEq1

-- | Generates a lambda expression which behaves like 'eq1' (without
-- requiring an 'StableNameEq1' instance).
makeStableNameEq1 :: Name -> Q Exp
makeStableNameEq1 name = makeLiftStableNameEq name `appE` varE eqValName

-- | Generates an 'StableNameEq2' instance declaration for the given data type or data
-- family instance.
--
-- This function is not available with @transformers-0.4@.
deriveStableNameEq2 :: Name -> Q [Dec]
deriveStableNameEq2 = deriveStableNameEqClass StableNameEq2

-- | Generates a lambda expression which behaves like 'liftStableNameEq2' (without
-- requiring an 'StableNameEq2' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftStableNameEq2 :: Name -> Q Exp
makeLiftStableNameEq2 = makeStableNameEqClass StableNameEq2

-- | Generates a lambda expression which behaves like 'eq2' (without
-- requiring an 'StableNameEq2' instance).
--
-- This function is not available with @transformers-0.4@.
makeStableNameEq2 :: Name -> Q Exp
makeStableNameEq2 name = makeLiftStableNameEq name `appE` varE eqValName `appE` varE eqValName

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive an StableNameEq(1)(2) instance declaration (depending on the StableNameEqClass
-- argument's value).
deriveStableNameEqClass :: StableNameEqClass -> Name -> Q [Dec]
deriveStableNameEqClass eClass name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext = ctxt
                 , datatypeName    = parentName
                 , datatypeVars    = vars
                 , datatypeVariant = variant
                 , datatypeCons    = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance eClass parentName ctxt vars variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (eqDecs eClass vars cons)

-- | Generates a declaration defining the primary function corresponding to a
-- particular class ((==$) for StableNameEq, liftStableNameEq for StableNameEq1, and
-- liftStableNameEq2 for StableNameEq2).
eqDecs :: StableNameEqClass -> [Type] -> [ConstructorInfo] -> [Q Dec]
eqDecs eClass vars cons =
    [ funD (eqName eClass)
           [ clause []
                    (normalB $ makeStableNameEqForCons eClass vars cons)
                    []
           ]
    ]

-- | Generates a lambda expression which behaves like (==) (for StableNameEq),
-- liftStableNameEq (for StableNameEq1), or liftStableNameEq2 (for StableNameEq2).
makeStableNameEqClass :: StableNameEqClass -> Name -> Q Exp
makeStableNameEqClass eClass name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext = ctxt
                 , datatypeName    = parentName
                 , datatypeVars    = vars
                 , datatypeVariant = variant
                 , datatypeCons    = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have (==$)/liftStableNameEq/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance eClass parentName ctxt vars variant
        >> makeStableNameEqForCons eClass vars cons

-- | Generates a lambda expression for (==$)/liftStableNameEq/etc. for the
-- given constructors. All constructors must be from the same type.
makeStableNameEqForCons :: StableNameEqClass -> [Type] -> [ConstructorInfo] -> Q Exp
makeStableNameEqForCons eClass vars cons = do
    value1 <- newName "value1"
    value2 <- newName "value2"
    eqDefn <- newName "eqDefn"
    eqs    <- newNameList "eq" $ arity eClass

    let lastTyVars = map varTToName $ drop (length vars - fromEnum eClass) vars
        tvMap      = Map.fromList $ zipWith (\x y -> (x, OneName y)) lastTyVars eqs

    lamE (map varP $
                     eqs ++
                     [value1, value2]
         ) . appsE
         $ [ varE $ eqConstName eClass
           , letE [ funD eqDefn $ map (makeCaseForCon eClass tvMap) patMatchCons
                               ++ fallThroughCase
                  ] $ varE eqDefn `appE` varE value1 `appE` varE value2
           ]
             ++ map varE eqs
             ++ [varE value1, varE value2]
  where
    nullaryCons, nonNullaryCons :: [ConstructorInfo]
    (nullaryCons, nonNullaryCons) = partition isNullaryCon cons

    tagMatchCons, patMatchCons :: [ConstructorInfo]
    (tagMatchCons, patMatchCons)
      | length nullaryCons > 10 = (nullaryCons, nonNullaryCons)
      | otherwise               = ([],          cons)

    fallThroughCase :: [Q Clause]
    fallThroughCase
      | null tagMatchCons = case patMatchCons of
          []  -> [makeFallThroughCaseTrue]  -- No constructors: _ == _ = True
          [_] -> []                         -- One constructor: no fall-through case
          _   -> [makeFallThroughCaseFalse] -- Two or more constructors:
                                            --   _ == _ = False
      | otherwise = [makeTagCase]

makeTagCase :: Q Clause
makeTagCase = do
    a     <- newName "a"
    aHash <- newName "a#"
    b     <- newName "b"
    bHash <- newName "b#"
    clause (map varP [a,b])
           (normalB $ untagExpr [(a, aHash), (b, bHash)] $
               primOpAppExpr (varE aHash) eqIntHashValName (varE bHash)) []

makeFallThroughCaseFalse, makeFallThroughCaseTrue :: Q Clause
makeFallThroughCaseFalse = makeFallThroughCase falseDataName
makeFallThroughCaseTrue  = makeFallThroughCase trueDataName

makeFallThroughCase :: Name -> Q Clause
makeFallThroughCase dataName =
    clause [wildP, wildP] (normalB $ appE (varE 'pure) (conE dataName)) []

makeCaseForCon :: StableNameEqClass -> TyVarMap1 -> ConstructorInfo -> Q Clause
makeCaseForCon eClass tvMap
  (ConstructorInfo { constructorName = conName, constructorFields = ts }) = do
    ts' <- mapM resolveTypeSynonyms ts
    let tsLen = length ts'
    as <- newNameList "a" tsLen
    bs <- newNameList "b" tsLen
    clause [conP conName (map varP as), conP conName (map varP bs)]
           (normalB $ makeCaseForArgs eClass tvMap conName ts' as bs)
           []

makeCaseForArgs :: StableNameEqClass
                -> TyVarMap1
                -> Name
                -> [Type]
                -> [Name]
                -> [Name]
                -> Q Exp
makeCaseForArgs _ _ _ [] [] [] = appE (varE 'pure) (conE trueDataName)
makeCaseForArgs eClass tvMap conName tys as bs =
    foldl1' (\q e -> appsE [(appE (varE 'liftA2) (varE andValName)), q, e])
            (zipWith3 (makeCaseForArg eClass tvMap conName) tys as bs)

makeCaseForArg :: StableNameEqClass
               -> TyVarMap1
               -> Name
               -> Type
               -> Name
               -> Name
               -> Q Exp
makeCaseForArg _ _ _ (ConT tyName) a b = primStableNameEqExpr
  where
    aExpr, bExpr :: Q Exp
    aExpr = varE a
    bExpr = varE b

    makePrimStableNameEqExpr :: Name -> Q Exp
    makePrimStableNameEqExpr n = appE (varE 'pure) (primOpAppExpr aExpr n bExpr)

    primStableNameEqExpr :: Q Exp
    primStableNameEqExpr
      | tyName == addrHashTypeName   = makePrimStableNameEqExpr eqAddrHashValName
      | tyName == charHashTypeName   = makePrimStableNameEqExpr eqCharHashValName
      | tyName == doubleHashTypeName = makePrimStableNameEqExpr eqDoubleHashValName
      | tyName == floatHashTypeName  = makePrimStableNameEqExpr eqFloatHashValName
      | tyName == intHashTypeName    = makePrimStableNameEqExpr eqIntHashValName
      | tyName == wordHashTypeName   = makePrimStableNameEqExpr eqWordHashValName
      | otherwise = infixApp aExpr (varE '($==)) bExpr
makeCaseForArg eClass tvMap conName ty a b =
    makeCaseForType eClass tvMap conName ty `appE` varE a `appE` varE b

makeCaseForType :: StableNameEqClass
                -> TyVarMap1
                -> Name
                -> Type
                -> Q Exp
makeCaseForType _ tvMap _ (VarT tyName) =
    varE $ case Map.lookup tyName tvMap of
      Just (OneName eq) -> eq
      Nothing           -> '($==)
makeCaseForType eClass tvMap conName (SigT ty _)      = makeCaseForType eClass tvMap conName ty
makeCaseForType eClass tvMap conName (ForallT _ _ ty) = makeCaseForType eClass tvMap conName ty
makeCaseForType eClass tvMap conName ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon:tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arity eClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isTyFamily tyCon
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError eClass conName
       else if any (`mentionsName` tyVarNames) rhsArgs
               then appsE $ [ varE . eqName $ toEnum numLastArgs]
                            ++ map (makeCaseForType eClass tvMap conName) rhsArgs
               else varE '($==)

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which @StableNameEq@ variant is being derived.
data StableNameEqClass =
     StableNameEq
   | StableNameEq1
   | StableNameEq2
     deriving (Bounded, Enum)

instance ClassRep StableNameEqClass where
    arity = fromEnum

    allowExQuant _ = True

    fullClassName StableNameEq  = ''StableNameEq
    fullClassName StableNameEq1 = ''StableNameEq1
    fullClassName StableNameEq2 = ''StableNameEq2

    classConstraint eClass i
      | eMin <= i && i <= eMax = Just $ fullClassName (toEnum i :: StableNameEqClass)
      | otherwise              = Nothing
      where
        eMin, eMax :: Int
        eMin = fromEnum (minBound :: StableNameEqClass)
        eMax = fromEnum eClass

eqConstName :: StableNameEqClass -> Name
eqConstName StableNameEq  = 'eqConst
eqConstName StableNameEq1 = 'liftEqConst
eqConstName StableNameEq2 = 'liftEq2Const

eqName :: StableNameEqClass -> Name
eqName StableNameEq  = '(==$)
eqName StableNameEq1 = 'liftStableNameEq
eqName StableNameEq2 = 'liftStableNameEq2
