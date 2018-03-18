{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Eq.StableName.Instances ( ) where

import Control.Applicative (liftA2, ZipList)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import Control.Exception
import Data.Char (GeneralCategory)
import Data.Complex (Complex)
import Data.Data (TyCon, Fixity, Constr, Proxy, ConstrRep, DataRep)
import Data.Eq.StableName.Class (StableNameEq, (==$), ($==))
import Data.Eq.StableName.TH (deriveStableNameEq)
import Data.Fixed (Fixed)
import Data.Function (on)
import Data.Functor.Compose (Compose(Compose), getCompose)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Functor.Product (Product(Pair))
import Data.Functor.Sum (Sum(InL, InR))
import Data.IORef (IORef)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down)
import Data.Ratio (Ratio)
import Data.STRef (STRef)
import Data.Type.Coercion (Coercion)
import Data.Type.Equality ((:~:), (:~~:))
import Data.Unique (Unique)
import Data.Version (Version)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Error (Errno)
import Foreign.C.Types
import Foreign.Ptr (IntPtr, WordPtr, Ptr, FunPtr)
import GHC.Conc (BlockReason, ThreadId, TVar, ThreadStatus)
import GHC.Event (Lifetime, TimeoutKey, FdKey, Event)
import GHC.ExecutionStack (SrcLoc)
import GHC.Exts (SpecConstrAnnotation)
import GHC.Fingerprint (Fingerprint)
import GHC.ForeignPtr (ForeignPtr)
import GHC.Generics
import GHC.IO.Buffer (BufferState)
import GHC.IO.Device (SeekMode, IODeviceType)
import GHC.IO.Encoding.Types (CodingProgress)
import GHC.IO.Handle (Newline, Handle, NewlineMode, BufferMode, HandlePosn)
import GHC.Stable (StablePtr)
import GHC.TypeLits (SomeSymbol)
import GHC.TypeNats (SomeNat)
import GHC.Types (Module, TrName)
import Numeric.Natural (Natural)
import System.Exit (ExitCode)
import System.IO (IOMode)
import System.IO.Error (IOErrorType)
import System.Mem.StableName (StableName)
import System.Posix.Types
import Text.Read.Lex (Number, Lexeme)
import Type.Reflection (SomeTypeRep, TypeRep)
import qualified Data.Monoid
import qualified Data.Semigroup

instance StableNameEq Bool                        where (==$) = eq
instance StableNameEq Ordering                    where (==$) = eq
instance StableNameEq Char                        where (==$) = eq
instance StableNameEq Float                       where (==$) = eq
instance StableNameEq Double                      where (==$) = eq
instance StableNameEq Int                         where (==$) = eq
instance StableNameEq Int8                        where (==$) = eq
instance StableNameEq Int16                       where (==$) = eq
instance StableNameEq Int32                       where (==$) = eq
instance StableNameEq Int64                       where (==$) = eq
instance StableNameEq Integer                     where (==$) = eq
instance StableNameEq Natural                     where (==$) = eq
instance StableNameEq Word                        where (==$) = eq
instance StableNameEq Word8                       where (==$) = eq
instance StableNameEq Word16                      where (==$) = eq
instance StableNameEq Word32                      where (==$) = eq
instance StableNameEq Word64                      where (==$) = eq
instance StableNameEq SomeTypeRep                 where (==$) = eq
instance StableNameEq TyCon                       where (==$) = eq
instance StableNameEq Module                      where (==$) = eq
instance StableNameEq TrName                      where (==$) = eq
instance StableNameEq GeneralCategory             where (==$) = eq
instance StableNameEq IOMode                      where (==$) = eq
instance StableNameEq IntPtr                      where (==$) = eq
instance StableNameEq WordPtr                     where (==$) = eq
instance StableNameEq CUIntMax                    where (==$) = eq
instance StableNameEq CIntMax                     where (==$) = eq
instance StableNameEq CUIntPtr                    where (==$) = eq
instance StableNameEq CIntPtr                     where (==$) = eq
instance StableNameEq CSUSeconds                  where (==$) = eq
instance StableNameEq CUSeconds                   where (==$) = eq
instance StableNameEq CTime                       where (==$) = eq
instance StableNameEq CClock                      where (==$) = eq
instance StableNameEq CSigAtomic                  where (==$) = eq
instance StableNameEq CWchar                      where (==$) = eq
instance StableNameEq CSize                       where (==$) = eq
instance StableNameEq CPtrdiff                    where (==$) = eq
instance StableNameEq CDouble                     where (==$) = eq
instance StableNameEq CFloat                      where (==$) = eq
instance StableNameEq CBool                       where (==$) = eq
instance StableNameEq CULLong                     where (==$) = eq
instance StableNameEq CLLong                      where (==$) = eq
instance StableNameEq CULong                      where (==$) = eq
instance StableNameEq CLong                       where (==$) = eq
instance StableNameEq CUInt                       where (==$) = eq
instance StableNameEq CInt                        where (==$) = eq
instance StableNameEq CUShort                     where (==$) = eq
instance StableNameEq CShort                      where (==$) = eq
instance StableNameEq CUChar                      where (==$) = eq
instance StableNameEq CSChar                      where (==$) = eq
instance StableNameEq CChar                       where (==$) = eq
instance StableNameEq SomeNat                     where (==$) = eq
instance StableNameEq SomeSymbol                  where (==$) = eq
instance StableNameEq DecidedStrictness           where (==$) = eq
instance StableNameEq SourceStrictness            where (==$) = eq
instance StableNameEq SourceUnpackedness          where (==$) = eq
instance StableNameEq Associativity               where (==$) = eq
instance StableNameEq Data.Monoid.Any             where (==$) = eq
instance StableNameEq Data.Monoid.All             where (==$) = eq
instance StableNameEq ArithException              where (==$) = eq
instance StableNameEq IOException                 where (==$) = eq
instance StableNameEq MaskingState                where (==$) = eq
instance StableNameEq BufferState                 where (==$) = eq
instance StableNameEq CodingProgress              where (==$) = eq
instance StableNameEq SeekMode                    where (==$) = eq
instance StableNameEq IODeviceType                where (==$) = eq
instance StableNameEq Newline                     where (==$) = eq
instance StableNameEq Handle                      where (==$) = eq
instance StableNameEq IOErrorType                 where (==$) = eq
instance StableNameEq AsyncException              where (==$) = eq
instance StableNameEq ArrayException              where (==$) = eq
instance StableNameEq Errno                       where (==$) = eq
instance StableNameEq Fd                          where (==$) = eq
instance StableNameEq CTimer                      where (==$) = eq
instance StableNameEq CKey                        where (==$) = eq
instance StableNameEq CId                         where (==$) = eq
instance StableNameEq CFsFilCnt                   where (==$) = eq
instance StableNameEq CFsBlkCnt                   where (==$) = eq
instance StableNameEq CClockId                    where (==$) = eq
instance StableNameEq CBlkCnt                     where (==$) = eq
instance StableNameEq CBlkSize                    where (==$) = eq
instance StableNameEq CRLim                       where (==$) = eq
instance StableNameEq CTcflag                     where (==$) = eq
instance StableNameEq CSpeed                      where (==$) = eq
instance StableNameEq CCc                         where (==$) = eq
instance StableNameEq CUid                        where (==$) = eq
instance StableNameEq CNlink                      where (==$) = eq
instance StableNameEq CGid                        where (==$) = eq
instance StableNameEq CSsize                      where (==$) = eq
instance StableNameEq CPid                        where (==$) = eq
instance StableNameEq COff                        where (==$) = eq
instance StableNameEq CMode                       where (==$) = eq
instance StableNameEq CIno                        where (==$) = eq
instance StableNameEq CDev                        where (==$) = eq
instance StableNameEq Lifetime                    where (==$) = eq
instance StableNameEq Event                       where (==$) = eq
instance StableNameEq BlockReason                 where (==$) = eq
instance StableNameEq ThreadId                    where (==$) = eq
instance StableNameEq TimeoutKey                  where (==$) = eq
instance StableNameEq Unique                      where (==$) = eq
instance StableNameEq Data.Data.Fixity            where (==$) = eq
instance StableNameEq Constr                      where (==$) = eq
instance StableNameEq SpecConstrAnnotation        where (==$) = eq
instance StableNameEq Void                        where (==$) = eq
instance StableNameEq (StablePtr a)               where (==$) = eq
instance StableNameEq (Ptr a)                     where (==$) = eq
instance StableNameEq (FunPtr a)                  where (==$) = eq
instance StableNameEq (MVar a)                    where (==$) = eq
instance StableNameEq (IORef a)                   where (==$) = eq
instance StableNameEq (ForeignPtr a)              where (==$) = eq
instance StableNameEq (TVar a)                    where (==$) = eq
instance StableNameEq (StableName a)              where (==$) = eq
instance StableNameEq (Fixed a)                   where (==$) = eq
instance StableNameEq (V1 p)                      where (==$) = eq
instance StableNameEq (U1 p)                      where (==$) = eq
instance StableNameEq (Type.Reflection.TypeRep a) where (==$) = eq
instance StableNameEq (STRef s a)                 where (==$) = eq
instance StableNameEq (Proxy a)                   where (==$) = eq
instance StableNameEq FdKey                       where (==$) = eq
instance StableNameEq (URec Word p)               where (==$) = eq
instance StableNameEq (URec Int p)                where (==$) = eq
instance StableNameEq (URec Float p)              where (==$) = eq
instance StableNameEq (URec Double p)             where (==$) = eq
instance StableNameEq (URec Char p)               where (==$) = eq
instance StableNameEq (URec (Ptr ()) p)           where (==$) = eq
instance StableNameEq (a :~: b)                   where (==$) = eq
instance StableNameEq (a :~~: b)                  where (==$) = eq
instance StableNameEq (Coercion a b)              where (==$) = eq

eq :: (Eq a) => a -> a -> IO Bool
eq x y = pure (x == y)

instance StableNameEq (f p) => StableNameEq (Rec1 f p) where
    (==$) = ($==) `on` unRec1

instance StableNameEq c => StableNameEq (K1 i c p) where
    (==$) = ($==) `on` unK1

instance StableNameEq (f p) => StableNameEq (M1 i c f p) where
    (==$) = ($==) `on` unM1

instance StableNameEq (f (g p)) => StableNameEq ((:.:) f g p) where
    (==$) = ($==) `on` unComp1

instance (StableNameEq (f p), StableNameEq (g p)) => StableNameEq ((:+:) f g p) where
    L1 f1 ==$ L1 f2 = f1 $== f2
    R1 g1 ==$ R1 g2 = g1 $== g2
    _     ==$ _     = pure False

instance (StableNameEq (f p), StableNameEq (g p)) => StableNameEq ((:*:) f g p) where
    (f1 :*: g1) ==$ (f2 :*: g2) = liftA2 (&&) (f1 $== f2) (g1 $== g2)

instance (StableNameEq (f a), StableNameEq (g a)) => StableNameEq (Data.Functor.Sum.Sum f g a) where
    Data.Functor.Sum.InL f1 ==$ Data.Functor.Sum.InL f2 = f1 $== f2
    Data.Functor.Sum.InR g1 ==$ Data.Functor.Sum.InR g2 = g1 $== g2
    _                       ==$ _      = pure False

instance (StableNameEq (f a), StableNameEq (g a)) => StableNameEq (Product f g a) where
    Pair f1 g1 ==$ Pair f2 g2 = liftA2 (&&) (f1 $== f2) (g1 $== g2)

instance StableNameEq (f (g a)) => StableNameEq (Compose f g a) where
    (==$) = ($==) `on` getCompose

instance (StableNameEq a) => StableNameEq (Data.Semigroup.Arg a b) where
    Data.Semigroup.Arg a _ ==$ Data.Semigroup.Arg b _ = a $== b

concat <$> traverse deriveStableNameEq
  [ ''()
  , ''(,)
  , ''(,,)
  , ''(,,,)
  , ''(,,,,)
  , ''(,,,,,)
  , ''(,,,,,,)
  , ''(,,,,,,,)
  , ''(,,,,,,,,)
  , ''(,,,,,,,,,)
  , ''(,,,,,,,,,,)
  , ''(,,,,,,,,,,,)
  , ''(,,,,,,,,,,,,)
  , ''(,,,,,,,,,,,,,)
  , ''(,,,,,,,,,,,,,,)
  , ''[]
  , ''Maybe
  , ''SrcLoc
  , ''Number
  , ''Lexeme
  , ''Fingerprint
  , ''GHC.Generics.Fixity
  , ''ErrorCall
  , ''NewlineMode
  , ''BufferMode
  , ''ExitCode
  , ''ThreadStatus
  , ''HandlePosn
  , ''Version
  , ''ConstrRep
  , ''DataRep
  , ''Ratio
  , ''Par1
  , ''Down
  , ''Data.Monoid.Last
  , ''Data.Monoid.First
  , ''Data.Monoid.Product
  , ''Data.Monoid.Sum
  , ''Data.Monoid.Dual
  , ''Identity
  , ''ZipList
  , ''Chan
  , ''NonEmpty
  , ''Data.Semigroup.Option
  , ''Data.Semigroup.WrappedMonoid
  , ''Data.Semigroup.Last
  , ''Data.Semigroup.First
  , ''Data.Semigroup.Max
  , ''Data.Semigroup.Min
  , ''Complex
  , ''Either
  , ''Const
  ]
