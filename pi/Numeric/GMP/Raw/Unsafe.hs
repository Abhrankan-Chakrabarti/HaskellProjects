{-# LANGUAGE ForeignFunctionInterface #-}
-- | Raw GMP foreign bindings, imported unsafe.
module Numeric.GMP.Raw.Unsafe where

import Foreign.Ptr (Ptr)
import Foreign.C.Types
import Numeric.GMP.Types

-- * Types for Documentation

type SrcPtr = Ptr
type VolatilePtr = Ptr
type VolatileSrcPtr = Ptr

-- * Integer Functions
-- ** Initialization Functions
foreign import ccall unsafe "__gmpz_init" mpz_init :: Ptr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_init2" mpz_init2 :: Ptr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_clear" mpz_clear :: Ptr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_realloc2" mpz_realloc2 :: Ptr MPZ -> MPBitCnt -> IO ()
-- ** Assignment Functions
foreign import ccall unsafe "__gmpz_set" mpz_set :: Ptr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_set_ui" mpz_set_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_set_si" mpz_set_si :: Ptr MPZ -> CLong -> IO ()
foreign import ccall unsafe "__gmpz_set_d" mpz_set_d :: Ptr MPZ -> CDouble -> IO ()
foreign import ccall unsafe "__gmpz_set_q" mpz_set_q :: Ptr MPZ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpz_set_f" mpz_set_f :: Ptr MPZ -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpz_set_str" mpz_set_str :: Ptr MPZ -> SrcPtr CChar -> CInt -> IO CInt
foreign import ccall unsafe "__gmpz_swap" mpz_swap :: Ptr MPZ -> Ptr MPZ -> IO ()
-- ** Combined Initialization and Assignment Functions
foreign import ccall unsafe "__gmpz_init_set" mpz_init_set :: Ptr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_init_set_ui" mpz_init_set_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_init_set_si" mpz_init_set_si :: Ptr MPZ -> CLong -> IO ()
foreign import ccall unsafe "__gmpz_init_set_d" mpz_init_set_d :: Ptr MPZ -> CDouble -> IO ()
foreign import ccall unsafe "__gmpz_init_set_str" mpz_init_set_str :: Ptr MPZ -> SrcPtr CChar -> CInt -> IO CInt
-- ** Conversion Functions
foreign import ccall unsafe "__gmpz_get_ui" mpz_get_ui :: SrcPtr MPZ -> IO CLong
foreign import ccall unsafe "__gmpz_get_si" mpz_get_si :: SrcPtr MPZ -> IO CLong
foreign import ccall unsafe "__gmpz_get_d" mpz_get_d :: SrcPtr MPZ -> IO CDouble
foreign import ccall unsafe "__gmpz_get_d_2exp" mpz_get_d_2exp :: Ptr CLong -> SrcPtr MPZ -> IO CDouble
foreign import ccall unsafe "__gmpz_get_str" mpz_get_str :: Ptr CChar -> CInt -> SrcPtr MPZ -> IO CChar
-- ** Arithmetic Functions
foreign import ccall unsafe "__gmpz_add" mpz_add :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_add_ui" mpz_add_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_sub" mpz_sub :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_sub_ui" mpz_sub_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_ui_sub" mpz_ui_sub :: Ptr MPZ -> CULong -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_mul" mpz_mul :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_mul_si" mpz_mul_si :: Ptr MPZ -> SrcPtr MPZ -> CLong -> IO ()
foreign import ccall unsafe "__gmpz_mul_ui" mpz_mul_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_addmul" mpz_addmul :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_addmul_ui" mpz_addmul_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_submul" mpz_submul :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_submul_ui" mpz_submul_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_mul_2exp" mpz_mul_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
-- mpz_neg
-- mpz_abs
-- ** Division Functions
foreign import ccall unsafe "__gmpz_cdiv_q" mpz_cdiv_q :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_cdiv_r" mpz_cdiv_r :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_cdiv_qr" mpz_cdiv_qr :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_cdiv_q_ui" mpz_cdiv_q_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_cdiv_r_ui" mpz_cdiv_r_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_cdiv_qr_ui" mpz_cdiv_qr_ui :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_cdiv_ui" mpz_cdiv_ui :: SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_cdiv_q_2exp" mpz_cdiv_q_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_cdiv_r_2exp" mpz_cdiv_r_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_fdiv_q" mpz_fdiv_q :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_fdiv_r" mpz_fdiv_r :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_fdiv_qr" mpz_fdiv_qr :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_fdiv_q_ui" mpz_fdiv_q_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_fdiv_r_ui" mpz_fdiv_r_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_fdiv_qr_ui" mpz_fdiv_qr_ui :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_fdiv_ui" mpz_fdiv_ui :: SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_fdiv_r_2exp" mpz_fdiv_r_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_fdiv_q_2exp" mpz_fdiv_q_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_tdiv_q" mpz_tdiv_q :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_tdiv_r" mpz_tdiv_r :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_tdiv_qr" mpz_tdiv_qr :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_tdiv_q_ui" mpz_tdiv_q_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_tdiv_r_ui" mpz_tdiv_r_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_tdiv_qr_ui" mpz_tdiv_qr_ui :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_tdiv_ui" mpz_tdiv_ui :: SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_tdiv_q_2exp" mpz_tdiv_q_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_tdiv_r_2exp" mpz_tdiv_r_2exp :: Ptr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_mod" mpz_mod :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
-- mpz_mod_ui
foreign import ccall unsafe "__gmpz_divexact" mpz_divexact :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_divexact_ui" mpz_divexact_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_divisible_p" mpz_divisible_p :: SrcPtr MPZ -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_divisible_ui_p" mpz_divisible_ui_p :: SrcPtr MPZ -> CULong -> IO CInt
foreign import ccall unsafe "__gmpz_divisible_2exp_p" mpz_divisible_2exp_p :: SrcPtr MPZ -> MPBitCnt -> IO CInt
foreign import ccall unsafe "__gmpz_congruent_p" mpz_congruent_p :: SrcPtr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_congruent_ui_p" mpz_congruent_ui_p :: SrcPtr MPZ -> CULong -> CULong -> IO CInt
foreign import ccall unsafe "__gmpz_congruent_2exp_p" mpz_congruent_2exp_p :: SrcPtr MPZ -> SrcPtr MPZ -> MPBitCnt -> IO CInt
-- ** Exponentiation Functions
foreign import ccall unsafe "__gmpz_powm" mpz_powm :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_powm_ui" mpz_powm_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_powm_sec" mpz_powm_sec :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_pow_ui" mpz_pow_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_ui_pow_ui" mpz_ui_pow_ui :: Ptr MPZ -> CULong -> CULong -> IO ()
-- ** Root Extraction Functions
foreign import ccall unsafe "__gmpz_root" mpz_root :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CInt
foreign import ccall unsafe "__gmpz_rootrem" mpz_rootrem :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_sqrt" mpz_sqrt :: Ptr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_sqrtrem" mpz_sqrtrem :: Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_perfect_power_p" mpz_perfect_power_p :: SrcPtr MPZ -> IO CInt
-- mpz_perfect_square_p
-- ** Number Theoretic Functions
foreign import ccall unsafe "__gmpz_probab_prime_p" mpz_probab_prime_p :: SrcPtr MPZ -> CInt -> IO CInt
foreign import ccall unsafe "__gmpz_nextprime" mpz_nextprime :: Ptr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_gcd" mpz_gcd :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_gcd_ui" mpz_gcd_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO CULong
foreign import ccall unsafe "__gmpz_gcdext" mpz_gcdext :: Ptr MPZ -> Ptr MPZ -> Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_lcm" mpz_lcm :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_lcm_ui" mpz_lcm_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_invert" mpz_invert :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_jacobi" mpz_jacobi :: SrcPtr MPZ -> SrcPtr MPZ -> IO CInt
-- mpz_legendre
-- mpz_kronecker
foreign import ccall unsafe "__gmpz_kronecker_si" mpz_kronecker_si :: SrcPtr MPZ -> CLong -> IO CInt
foreign import ccall unsafe "__gmpz_kronecker_ui" mpz_kronecker_ui :: SrcPtr MPZ -> CULong -> IO CInt
foreign import ccall unsafe "__gmpz_si_kronecker" mpz_si_kronecker :: CLong -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_ui_kronecker" mpz_ui_kronecker :: CULong -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_remove" mpz_remove :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO MPBitCnt
foreign import ccall unsafe "__gmpz_fac_ui" mpz_fac_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_2fac_ui" mpz_2fac_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_mfac_uiui" mpz_mfac_uiui :: Ptr MPZ -> CULong -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_primorial_ui" mpz_primorial_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_bin_ui" mpz_bin_ui :: Ptr MPZ -> SrcPtr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_bin_uiui" mpz_bin_uiui :: Ptr MPZ -> CULong -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_fib_ui" mpz_fib_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_fib2_ui" mpz_fib2_ui :: Ptr MPZ -> Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_lucnum_ui" mpz_lucnum_ui :: Ptr MPZ -> CULong -> IO ()
foreign import ccall unsafe "__gmpz_lucnum2_ui" mpz_lucnum2_ui :: Ptr MPZ -> Ptr MPZ -> CULong -> IO ()
-- ** Comparison Functions
foreign import ccall unsafe "__gmpz_cmp" mpz_cmp :: SrcPtr MPZ -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_cmp_d" mpz_cmp_d :: SrcPtr MPZ -> CDouble -> IO CInt
foreign import ccall unsafe "__gmpz_cmp_si" mpz_cmp_si :: SrcPtr MPZ -> CLong -> IO CInt
foreign import ccall unsafe "__gmpz_cmp_ui" mpz_cmp_ui :: SrcPtr MPZ -> CULong -> IO CInt
foreign import ccall unsafe "__gmpz_cmpabs" mpz_cmpabs :: SrcPtr MPZ -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpz_cmpabs_d" mpz_cmpabs_d :: SrcPtr MPZ -> CDouble -> IO CInt
foreign import ccall unsafe "__gmpz_cmpabs_ui" mpz_cmpabs_ui :: SrcPtr MPZ -> CULong -> IO CInt
-- mpz_sgn
-- ** Logical and Bit Manipulation Functions
foreign import ccall unsafe "__gmpz_and" mpz_and :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_ior" mpz_ior :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_xor" mpz_xor :: Ptr MPZ -> SrcPtr MPZ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_com" mpz_com :: Ptr MPZ -> SrcPtr MPZ -> IO ()
-- mpz_popcount
foreign import ccall unsafe "__gmpz_hamdist" mpz_hamdist :: SrcPtr MPZ -> SrcPtr MPZ -> IO MPBitCnt
foreign import ccall unsafe "__gmpz_scan0" mpz_scan0 :: SrcPtr MPZ -> MPBitCnt -> IO MPBitCnt
foreign import ccall unsafe "__gmpz_scan1" mpz_scan1 :: SrcPtr MPZ -> MPBitCnt -> IO MPBitCnt
foreign import ccall unsafe "__gmpz_setbit" mpz_setbit :: Ptr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_clrbit" mpz_clrbit :: Ptr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_combit" mpz_combit :: Ptr MPZ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_tstbit" mpz_tstbit :: SrcPtr MPZ -> MPBitCnt -> IO CInt
-- ** Input and Output Functions
foreign import ccall unsafe "__gmpz_out_str" mpz_out_str :: Ptr CFile -> CInt -> SrcPtr MPZ -> IO CSize
foreign import ccall unsafe "__gmpz_inp_str" mpz_inp_str :: Ptr MPZ -> Ptr CFile -> CInt -> IO CSize
foreign import ccall unsafe "__gmpz_out_raw" mpz_out_raw :: Ptr CFile -> SrcPtr MPZ -> IO CSize
foreign import ccall unsafe "__gmpz_inp_raw" mpz_inp_raw :: Ptr MPZ -> Ptr CFile -> IO CSize
-- ** Random Number Functions
foreign import ccall unsafe "__gmpz_urandomb" mpz_urandomb :: Ptr MPZ -> Ptr GMPRandState -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpz_urandomm" mpz_urandomm :: Ptr MPZ -> Ptr GMPRandState -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_rrandomb" mpz_rrandomb :: Ptr MPZ -> Ptr GMPRandState -> MPBitCnt -> IO ()
-- ** Integer Import and Export
foreign import ccall unsafe "__gmpz_import" mpz_import :: Ptr MPZ -> CSize -> CInt -> CSize -> CInt -> CSize -> Ptr a -> IO ()
foreign import ccall unsafe "__gmpz_export" mpz_export :: Ptr a -> Ptr CSize -> CInt -> CSize -> CInt -> CSize -> SrcPtr MPZ -> IO ()
-- ** Miscellaneous Functions
-- mpz_fits_ulong_p
foreign import ccall unsafe "__gmpz_fits_slong_p" mpz_fits_slong_p :: SrcPtr MPZ -> IO CInt
-- mpz_fits_uint_p
foreign import ccall unsafe "__gmpz_fits_sint_p" mpz_fits_sint_p :: SrcPtr MPZ -> IO CInt
-- mpz_fits_ushort_p
foreign import ccall unsafe "__gmpz_fits_sshort_p" mpz_fits_sshort_p :: SrcPtr MPZ -> IO CInt
-- mpz_odd_p
-- mpz_even_p
foreign import ccall unsafe "__gmpz_sizeinbase" mpz_sizeinbase :: SrcPtr MPZ -> CInt -> IO CSize
-- ** Special Functions
foreign import ccall unsafe "__gmpz_realloc" mpz_realloc :: Ptr MPZ -> MPSize -> IO ()
-- mpz_get_limbn
-- mpz_size
foreign import ccall unsafe "__gmpz_limbs_read" mpz_limbs_read :: SrcPtr MPZ -> IO (SrcPtr MPLimb)
foreign import ccall unsafe "__gmpz_limbs_write" mpz_limbs_write :: Ptr MPZ -> MPSize -> IO (Ptr MPLimb)
foreign import ccall unsafe "__gmpz_limbs_modify" mpz_limbs_modify :: Ptr MPZ -> MPSize -> IO (Ptr MPLimb)
foreign import ccall unsafe "__gmpz_limbs_finish" mpz_limbs_finish :: Ptr MPZ -> MPSize -> IO ()
foreign import ccall unsafe "__gmpz_roinit_n" mpz_roinit_n :: Ptr MPZ -> SrcPtr MPLimb -> MPSize -> IO (SrcPtr MPZ)
-- MPZ_ROINIT_N

-- * Rational Number Functions
foreign import ccall unsafe "__gmpq_canonicalize" mpq_canonicalize :: Ptr MPQ -> IO ()
-- ** Initialization and Assignment Functions
foreign import ccall unsafe "__gmpq_init" mpq_init :: Ptr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_clear" mpq_clear :: Ptr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_set" mpq_set :: Ptr MPQ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_set_z" mpq_set_z :: Ptr MPQ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpq_set_ui" mpq_set_ui :: Ptr MPQ -> CULong -> CULong -> IO ()
foreign import ccall unsafe "__gmpq_set_si" mpq_set_si :: Ptr MPQ -> CLong -> CULong -> IO ()
foreign import ccall unsafe "__gmpq_set_str" mpq_set_str :: Ptr MPQ -> SrcPtr CChar -> CInt -> IO CInt
foreign import ccall unsafe "__gmpq_swap" mpq_swap :: Ptr MPQ -> Ptr MPQ -> IO ()
-- ** Conversion Functions
foreign import ccall unsafe "__gmpq_get_d" mpq_get_d :: SrcPtr MPQ -> IO CDouble
foreign import ccall unsafe "__gmpq_set_d" mpq_set_d :: Ptr MPQ -> CDouble -> IO ()
foreign import ccall unsafe "__gmpq_set_f" mpq_set_f :: Ptr MPQ -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpq_get_str" mpq_get_str :: Ptr CChar -> CInt -> SrcPtr MPQ -> IO (Ptr CChar)
-- ** Arithmetic Functions
foreign import ccall unsafe "__gmpq_add" mpq_add :: Ptr MPQ -> SrcPtr MPQ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_sub" mpq_sub :: Ptr MPQ -> SrcPtr MPQ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_mul" mpq_mul :: Ptr MPQ -> SrcPtr MPQ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_mul_2exp" mpq_mul_2exp :: Ptr MPQ -> SrcPtr MPQ -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpq_div" mpq_div :: Ptr MPQ -> SrcPtr MPQ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_div_2exp" mpq_div_2exp :: Ptr MPQ -> SrcPtr MPQ -> MPBitCnt -> IO ()
-- mpq_neg
-- mpq_abs
foreign import ccall unsafe "__gmpq_inv" mpq_inv :: Ptr MPQ -> SrcPtr MPQ -> IO ()
-- ** Comparison Functions
foreign import ccall unsafe "__gmpq_cmp" mpq_cmp :: SrcPtr MPQ -> SrcPtr MPQ -> IO CInt
foreign import ccall unsafe "__gmpq_cmp_z" mpq_cmp_z :: SrcPtr MPQ -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpq_cmp_ui" mpq_cmp_ui :: SrcPtr MPQ -> CULong -> CULong -> IO CInt
foreign import ccall unsafe "__gmpq_cmp_si" mpq_cmp_si :: SrcPtr MPQ -> CLong -> CULong -> IO CInt
-- mpq_sgn
foreign import ccall unsafe "__gmpq_equal" mpq_equal :: SrcPtr MPQ -> SrcPtr MPQ -> IO CInt
-- ** Applying Integer Functions to Rationals
-- See also 'mpq_numref' and 'mpq_denref'.
foreign import ccall unsafe "__gmpq_get_num" mpq_get_num :: Ptr MPZ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_get_den" mpq_get_den :: Ptr MPZ -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpq_set_num" mpq_set_num :: Ptr MPQ -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpq_set_den" mpq_set_den :: Ptr MPQ -> SrcPtr MPZ -> IO ()
-- ** Input and Output Functions
foreign import ccall unsafe "__gmpq_out_str" mpq_out_str :: Ptr CFile -> CInt -> SrcPtr MPQ -> IO CSize
foreign import ccall unsafe "__gmpq_inp_str" mpq_inp_str :: Ptr MPQ -> Ptr CFile -> CInt -> IO CSize

-- * Floating-point Functions
-- ** Initialization Functions
{-
-- not thread-safe, ie, requires running everything in a bound thread for expected behaviour
foreign import ccall unsafe "__gmpf_set_default_prec" mpf_set_default_prec :: MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpf_get_default_prec" mpf_get_default_prec :: IO MPBitCnt
foreign import ccall unsafe "__gmpf_init" mpf_init :: Ptr MPF -> IO ()
-}
foreign import ccall unsafe "__gmpf_init2" mpf_init2 :: Ptr MPF -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpf_clear" mpf_clear :: Ptr MPF -> IO ()
foreign import ccall unsafe "__gmpf_get_prec" mpf_get_prec :: SrcPtr MPF -> IO MPBitCnt
foreign import ccall unsafe "__gmpf_set_prec" mpf_set_prec :: Ptr MPF -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpf_set_prec_raw" mpf_set_prec_raw :: Ptr MPF -> MPBitCnt -> IO ()
-- ** Assignment Functions
foreign import ccall unsafe "__gmpf_set" mpf_set :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_set_ui" mpf_set_ui :: Ptr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_set_si" mpf_set_si :: Ptr MPF -> CLong -> IO ()
foreign import ccall unsafe "__gmpf_set_d" mpf_set_d :: Ptr MPF -> CDouble -> IO ()
foreign import ccall unsafe "__gmpf_set_z" mpf_set_z :: Ptr MPF -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpf_set_q" mpf_set_q :: Ptr MPF -> SrcPtr MPQ -> IO ()
foreign import ccall unsafe "__gmpf_set_str" mpf_set_str :: Ptr MPF -> SrcPtr CChar -> CInt -> IO CInt
foreign import ccall unsafe "__gmpf_swap" mpf_swap :: Ptr MPF -> Ptr MPF -> IO ()
-- ** Combined Initialization and Assignment Functions
foreign import ccall unsafe "__gmpf_init_set" mpf_init_set :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_init_set_ui" mpf_init_set_ui :: Ptr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_init_set_si" mpf_init_set_si :: Ptr MPF -> CLong -> IO ()
foreign import ccall unsafe "__gmpf_init_set_d" mpf_init_set_d :: Ptr MPF -> CDouble -> IO ()
foreign import ccall unsafe "__gmpf_init_set_str" mpf_init_set_str :: Ptr MPF -> SrcPtr CChar -> CInt -> IO CInt
-- ** Conversion Functions
foreign import ccall unsafe "__gmpf_get_d" mpf_get_d :: SrcPtr MPF -> IO CDouble
foreign import ccall unsafe "__gmpf_get_d_2exp" mpf_get_d_2exp :: Ptr CLong -> SrcPtr MPF -> IO CDouble
foreign import ccall unsafe "__gmpf_get_si" mpf_get_si :: SrcPtr MPF -> IO CLong
foreign import ccall unsafe "__gmpf_get_ui" mpf_get_ui :: SrcPtr MPF -> IO CULong
foreign import ccall unsafe "__gmpf_get_str" mpf_get_str :: Ptr CChar -> Ptr MPExp -> CInt -> CSize -> SrcPtr MPF -> IO (Ptr CChar)
-- ** Arithmetic Functions
foreign import ccall unsafe "__gmpf_add" mpf_add :: Ptr MPF -> SrcPtr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_add_ui" mpf_add_ui :: Ptr MPF -> SrcPtr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_sub" mpf_sub :: Ptr MPF -> SrcPtr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_sub_ui" mpf_sub_ui :: Ptr MPF -> SrcPtr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_ui_sub" mpf_ui_sub :: Ptr MPF -> CULong -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_mul" mpf_mul :: Ptr MPF -> SrcPtr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_mul_ui" mpf_mul_ui :: Ptr MPF -> SrcPtr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_div" mpf_div :: Ptr MPF -> SrcPtr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_ui_div" mpf_ui_div :: Ptr MPF -> CULong -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_div_ui" mpf_div_ui :: Ptr MPF -> SrcPtr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_sqrt" mpf_sqrt :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_sqrt_ui" mpf_sqrt_ui :: Ptr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_pow_ui" mpf_pow_ui :: Ptr MPF -> SrcPtr MPF -> CULong -> IO ()
foreign import ccall unsafe "__gmpf_neg" mpf_neg :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_abs" mpf_abs :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_mul_2exp" mpf_mul_2exp :: Ptr MPF -> SrcPtr MPF -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpf_div_2exp" mpf_div_2exp :: Ptr MPF -> SrcPtr MPF -> MPBitCnt -> IO ()
-- ** Comparison Functions
foreign import ccall unsafe "__gmpf_cmp" mpf_cmp :: SrcPtr MPF -> SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_cmp_z" mpf_cmp_z :: SrcPtr MPF -> SrcPtr MPZ -> IO CInt
foreign import ccall unsafe "__gmpf_cmp_d" mpf_cmp_d :: SrcPtr MPF -> CDouble -> IO CInt
foreign import ccall unsafe "__gmpf_cmp_ui" mpf_cmp_ui :: SrcPtr MPF -> CULong -> IO CInt
foreign import ccall unsafe "__gmpf_cmp_si" mpf_cmp_si :: SrcPtr MPF -> CLong -> IO CInt
foreign import ccall unsafe "__gmpf_reldiff" mpf_reldiff :: Ptr MPF -> SrcPtr MPF -> SrcPtr MPF -> IO ()
-- mpf_sgn
-- ** Input and Output Functions
foreign import ccall unsafe "__gmpf_out_str" mpf_out_str :: Ptr CFile -> CInt -> CSize -> SrcPtr MPF -> IO CSize
foreign import ccall unsafe "__gmpf_inp_str" mpf_inp_str :: Ptr MPF -> Ptr CFile -> CInt -> IO CSize
-- ** Miscellaneous Functions
foreign import ccall unsafe "__gmpf_ceil" mpf_ceil :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_floor" mpf_floor :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_trunc" mpf_trunc :: Ptr MPF -> SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_integer_p" mpf_integer_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_fits_ulong_p" mpf_fits_ulong_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_fits_slong_p" mpf_fits_slong_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_fits_uint_p" mpf_fits_uint_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_fits_sint_p" mpf_fits_sint_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_fits_ushort_p" mpf_fits_ushort_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_fits_sshort_p" mpf_fits_sshort_p :: SrcPtr MPF -> IO CInt
foreign import ccall unsafe "__gmpf_urandomb" mpf_urandomb :: Ptr MPF -> Ptr GMPRandState -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmpf_random2" mpf_random2 :: Ptr MPF -> MPSize -> MPExp -> IO ()

-- * Random Number Functions
-- ** Random State Initialization
foreign import ccall unsafe "__gmp_randinit_default" gmp_randinit_default :: Ptr GMPRandState -> IO ()
foreign import ccall unsafe "__gmp_randinit_mt" gmp_randinit_mt :: Ptr GMPRandState -> IO ()
foreign import ccall unsafe "__gmp_randinit_lc_2exp" gmp_randinit_lc_2exp :: Ptr GMPRandState -> SrcPtr MPZ -> CULong -> MPBitCnt -> IO ()
foreign import ccall unsafe "__gmp_randinit_lc_2exp_size" gmp_randinit_lc_2exp_size :: Ptr GMPRandState -> MPBitCnt -> IO CInt
foreign import ccall unsafe "__gmp_randinit_set" gmp_randinit_set :: Ptr GMPRandState -> SrcPtr GMPRandState -> IO ()
foreign import ccall unsafe "__gmp_randclear" gmp_randclear :: Ptr GMPRandState -> IO ()
-- ** Random State Seeding
foreign import ccall unsafe "__gmp_randseed" gmp_randseed :: Ptr GMPRandState -> SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmp_randseed_ui" gmp_randseed_ui :: Ptr GMPRandState -> CULong -> IO ()
-- ** Random State Miscellaneous
foreign import ccall unsafe "__gmp_urandomb_ui" gmp_urandomb_ui :: Ptr GMPRandState -> CULong -> IO CULong
foreign import ccall unsafe "__gmp_urandomm_ui" gmp_urandomm_ui :: Ptr GMPRandState -> CULong -> IO CULong

-- * Low-level Functions
foreign import ccall unsafe "__gmpn_add_n" mpn_add_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO MPLimb
foreign import ccall unsafe "__gmpn_addmul_1" mpn_addmul_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_divexact_1" mpn_divexact_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO ()
foreign import ccall unsafe "__gmpn_divexact_by3c" mpn_divexact_by3c :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_divrem" mpn_divrem :: Ptr MPLimb -> MPSize -> Ptr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> IO MPLimb
foreign import ccall unsafe "__gmpn_divrem_1" mpn_divrem_1 :: Ptr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_divrem_2" mpn_divrem_2 :: Ptr MPLimb -> MPSize -> Ptr MPLimb -> MPSize -> SrcPtr MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_div_qr_1" mpn_div_qr_1 :: Ptr MPLimb -> Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_div_qr_2" mpn_div_qr_2 :: Ptr MPLimb -> Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> SrcPtr MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_gcd" mpn_gcd :: Ptr MPLimb -> Ptr MPLimb -> MPSize -> Ptr MPLimb -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_gcd_1" mpn_gcd_1 :: SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_gcdext_1" mpn_gcdext_1 :: Ptr MPLimbSigned -> Ptr MPLimbSigned -> MPLimb -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_gcdext" mpn_gcdext :: Ptr MPLimb -> Ptr MPLimb -> Ptr MPSize -> Ptr MPLimb -> MPSize -> Ptr MPLimb -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_get_str" mpn_get_str :: Ptr CUChar -> CInt -> Ptr MPLimb -> MPSize -> IO CSize
foreign import ccall unsafe "__gmpn_hamdist" mpn_hamdist :: SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO MPBitCnt
foreign import ccall unsafe "__gmpn_lshift" mpn_lshift :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> CUInt -> IO MPLimb
foreign import ccall unsafe "__gmpn_mod_1" mpn_mod_1 :: SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_mul" mpn_mul :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> IO MPLimb
foreign import ccall unsafe "__gmpn_mul_1" mpn_mul_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_mul_n" mpn_mul_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_sqr" mpn_sqr :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_com" mpn_com :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_perfect_square_p" mpn_perfect_square_p :: SrcPtr MPLimb -> MPSize -> IO CInt
foreign import ccall unsafe "__gmpn_perfect_power_p" mpn_perfect_power_p :: SrcPtr MPLimb -> MPSize -> IO CInt
foreign import ccall unsafe "__gmpn_popcount" mpn_popcount :: SrcPtr MPLimb -> MPSize -> IO MPBitCnt
foreign import ccall unsafe "__gmpn_pow_1" mpn_pow_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> Ptr MPLimb -> IO MPSize
foreign import ccall unsafe "__gmpn_preinv_mod_1" mpn_preinv_mod_1 :: SrcPtr MPLimb -> MPSize -> MPLimb -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_random" mpn_random :: Ptr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_random2" mpn_random2 :: Ptr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_rshift" mpn_rshift :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> CUInt -> IO MPLimb
foreign import ccall unsafe "__gmpn_scan0" mpn_scan0 :: SrcPtr MPLimb -> MPBitCnt -> IO MPBitCnt
foreign import ccall unsafe "__gmpn_scan1" mpn_scan1 :: SrcPtr MPLimb -> MPBitCnt -> IO MPBitCnt
foreign import ccall unsafe "__gmpn_set_str" mpn_set_str :: Ptr MPLimb -> SrcPtr CChar -> CSize -> CInt -> IO MPSize
foreign import ccall unsafe "__gmpn_sizeinbase" mpn_sizeinbase :: SrcPtr MPLimb -> MPSize -> CInt -> IO CSize
foreign import ccall unsafe "__gmpn_sqrtrem" mpn_sqrtrem :: Ptr MPLimb -> Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sub_n" mpn_sub_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO MPLimb
foreign import ccall unsafe "__gmpn_submul_1" mpn_submul_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_tdiv_qr" mpn_tdiv_qr :: Ptr MPLimb -> Ptr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_and_n" mpn_and_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_andn_n" mpn_andn_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_nand_n" mpn_nand_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_ior_n" mpn_ior_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_iorn_n" mpn_iorn_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_nior_n" mpn_nior_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_xor_n" mpn_xor_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_xnor_n" mpn_xnor_n :: Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_copyi" mpn_copyi :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_copyd" mpn_copyd :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_zero" mpn_zero :: Ptr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_cnd_add_n" mpn_cnd_add_n :: MPLimb -> Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO MPLimb
foreign import ccall unsafe "__gmpn_cnd_sub_n" mpn_cnd_sub_n :: MPLimb -> Ptr MPLimb -> SrcPtr MPLimb -> SrcPtr MPLimb -> MPSize -> IO MPLimb
foreign import ccall unsafe "__gmpn_sec_add_1" mpn_sec_add_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> Ptr MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_sec_add_1_itch" mpn_sec_add_1_itch :: MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sec_sub_1" mpn_sec_sub_1 :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPLimb -> Ptr MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_sec_sub_1_itch" mpn_sec_sub_1_itch :: MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_cnd_swap" mpn_cnd_swap :: MPLimb -> VolatilePtr MPLimb -> VolatilePtr MPLimb -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_sec_mul" mpn_sec_mul :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> Ptr MPLimb -> IO ()
foreign import ccall unsafe "__gmpn_sec_mul_itch" mpn_sec_mul_itch :: MPSize -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sec_sqr" mpn_sec_sqr :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> Ptr MPLimb -> IO ()
foreign import ccall unsafe "__gmpn_sec_sqr_itch" mpn_sec_sqr_itch :: MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sec_powm" mpn_sec_powm :: Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> SrcPtr MPLimb -> MPBitCnt -> SrcPtr MPLimb -> MPSize -> Ptr MPLimb -> IO ()
foreign import ccall unsafe "__gmpn_sec_powm_itch" mpn_sec_powm_itch :: MPSize -> MPBitCnt -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sec_tabselect" mpn_sec_tabselect :: VolatilePtr MPLimb -> VolatileSrcPtr MPLimb -> MPSize -> MPSize -> MPSize -> IO ()
foreign import ccall unsafe "__gmpn_sec_div_qr" mpn_sec_div_qr :: Ptr MPLimb -> Ptr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> Ptr MPLimb -> IO MPLimb
foreign import ccall unsafe "__gmpn_sec_div_qr_itch" mpn_sec_div_qr_itch :: MPSize -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sec_div_r" mpn_sec_div_r :: Ptr MPLimb -> MPSize -> SrcPtr MPLimb -> MPSize -> Ptr MPLimb -> IO ()
foreign import ccall unsafe "__gmpn_sec_div_r_itch" mpn_sec_div_r_itch :: MPSize -> MPSize -> IO MPSize
foreign import ccall unsafe "__gmpn_sec_invert" mpn_sec_invert :: Ptr MPLimb -> Ptr MPLimb -> SrcPtr MPLimb -> MPSize -> MPBitCnt -> Ptr MPLimb -> IO CInt
foreign import ccall unsafe "__gmpn_sec_invert_itch" mpn_sec_invert_itch :: MPSize -> IO MPSize

{-
foreign import ccall unsafe "__gmpz_dump" mpz_dump :: SrcPtr MPZ -> IO ()
foreign import ccall unsafe "__gmpz_millerrabin" mpz_millerrabin :: SrcPtr MPZ -> CInt -> IO CInt
foreign import ccall unsafe "__gmpf_dump" mpf_dump :: SrcPtr MPF -> IO ()
foreign import ccall unsafe "__gmpf_eq" mpf_eq :: SrcPtr MPF -> SrcPtr MPF -> MPBitCnt -> IO CInt
foreign import ccall unsafe "__gmpf_size" mpf_size :: SrcPtr MPF -> IO CSize
-}
