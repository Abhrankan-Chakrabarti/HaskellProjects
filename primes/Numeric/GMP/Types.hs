{-# LINE 1 "Numeric/GMP/Types.hsc" #-}



{-# LINE 4 "Numeric/GMP/Types.hsc" #-}


{-# LINE 6 "Numeric/GMP/Types.hsc" #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | GMP types.
module Numeric.GMP.Types where

import Data.Data
import Data.Typeable
import Data.Bits
import Data.Ix
import Data.Int
import Data.Word

import Foreign (Storable(..), Ptr, nullPtr, plusPtr)
import Foreign.C (CInt)

-- | @mpz_t@
data MPZ = MPZ
  { mpzAlloc :: !CInt
  , mpzSize :: !CInt
  , mpzD :: !(Ptr MPLimb)
  }

instance Storable MPZ where
  sizeOf _ = ((16))
{-# LINE 32 "Numeric/GMP/Types.hsc" #-}
  alignment _ = (8)
{-# LINE 33 "Numeric/GMP/Types.hsc" #-}
  peek ptr = do
    alloc <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 35 "Numeric/GMP/Types.hsc" #-}
    size <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 36 "Numeric/GMP/Types.hsc" #-}
    d <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 37 "Numeric/GMP/Types.hsc" #-}
    return (MPZ{ mpzAlloc = alloc, mpzSize = size, mpzD = d })
  poke ptr (MPZ{ mpzAlloc = alloc, mpzSize = size, mpzD = d }) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr alloc
{-# LINE 40 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr size
{-# LINE 41 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr d
{-# LINE 42 "Numeric/GMP/Types.hsc" #-}

-- | @mpq_t@
data MPQ = MPQ
  { mpqNum :: !MPZ
  , mpqDen :: !MPZ
  }

instance Storable MPQ where
  sizeOf _ = ((32))
{-# LINE 51 "Numeric/GMP/Types.hsc" #-}
  alignment _ = (8)
{-# LINE 52 "Numeric/GMP/Types.hsc" #-}
  peek ptr = do
    num <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 54 "Numeric/GMP/Types.hsc" #-}
    den <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 55 "Numeric/GMP/Types.hsc" #-}
    return (MPQ{ mpqNum = num, mpqDen = den })
  poke ptr (MPQ{ mpqNum = num, mpqDen = den }) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr num
{-# LINE 58 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr den
{-# LINE 59 "Numeric/GMP/Types.hsc" #-}

-- | Get pointers to numerator and denominator (these are macros in the C API).
mpq_numref, mpq_denref :: Ptr MPQ -> Ptr MPZ
mpq_numref ptr = plusPtr ptr ((0))
{-# LINE 63 "Numeric/GMP/Types.hsc" #-}
mpq_denref ptr = plusPtr ptr ((16))
{-# LINE 64 "Numeric/GMP/Types.hsc" #-}

-- | @mpf_t@
data MPF = MPF
  { mpfPrec :: !CInt
  , mpfSize :: !CInt
  , mpfExp :: !MPExp
  , mpfD :: !(Ptr MPLimb)
  }

instance Storable MPF where
  sizeOf _ = ((24))
{-# LINE 75 "Numeric/GMP/Types.hsc" #-}
  alignment _ = (8)
{-# LINE 76 "Numeric/GMP/Types.hsc" #-}
  peek ptr = do
    prec <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 78 "Numeric/GMP/Types.hsc" #-}
    size <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 79 "Numeric/GMP/Types.hsc" #-}
    expo <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 80 "Numeric/GMP/Types.hsc" #-}
    d <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 81 "Numeric/GMP/Types.hsc" #-}
    return (MPF{ mpfPrec = prec, mpfSize = size, mpfExp = expo, mpfD = d })
  poke ptr (MPF{ mpfPrec = prec, mpfSize = size, mpfExp = expo, mpfD = d }) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr prec
{-# LINE 84 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr size
{-# LINE 85 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr expo
{-# LINE 86 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr d
{-# LINE 87 "Numeric/GMP/Types.hsc" #-}

-- | @gmp_randstate_t@
data GMPRandState = GMPRandState
  { gmprsSeed :: !MPZ
  , gmprsAlg :: !GMPRandAlg
  , gmprsAlgData :: !(Ptr ())
  }

instance Storable GMPRandState where
  sizeOf _ = ((32))
{-# LINE 97 "Numeric/GMP/Types.hsc" #-}
  alignment _ = (8)
{-# LINE 98 "Numeric/GMP/Types.hsc" #-}
  peek ptr = do
    seed <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 100 "Numeric/GMP/Types.hsc" #-}
    alg <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 101 "Numeric/GMP/Types.hsc" #-}
    algdata <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 102 "Numeric/GMP/Types.hsc" #-}
    return (GMPRandState{ gmprsSeed = seed, gmprsAlg = alg, gmprsAlgData = algdata })
  poke ptr (GMPRandState{ gmprsSeed = seed, gmprsAlg = alg, gmprsAlgData = algdata }) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr seed
{-# LINE 105 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr alg
{-# LINE 106 "Numeric/GMP/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr algdata
{-# LINE 107 "Numeric/GMP/Types.hsc" #-}

-- | @mp_limb_t@
newtype MPLimb = MPLimb (Word64)
{-# LINE 110 "Numeric/GMP/Types.hsc" #-}
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_limb_signed_t@
newtype MPLimbSigned = MPLimbSigned (Int64)
{-# LINE 114 "Numeric/GMP/Types.hsc" #-}
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_size_t@
newtype MPSize = MPSize (Int64)
{-# LINE 118 "Numeric/GMP/Types.hsc" #-}
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_exp_t@
newtype MPExp = MPExp (Int64)
{-# LINE 122 "Numeric/GMP/Types.hsc" #-}
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_bitcnt_t@
newtype MPBitCnt = MPBitCnt (Word64)
{-# LINE 126 "Numeric/GMP/Types.hsc" #-}
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @gmp_randalg_t@
newtype GMPRandAlg = GMPRandAlg (Word32)
{-# LINE 130 "Numeric/GMP/Types.hsc" #-}
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)
