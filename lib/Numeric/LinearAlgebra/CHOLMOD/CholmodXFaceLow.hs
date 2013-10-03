--------------------------------------------------------------------------------
--
--  Copyright (c) 2010 - 2013 Tad Doxsee
--  MIT License
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE EmptyDataDecls           #-}


module Numeric.LinearAlgebra.CHOLMOD.CholmodXFaceLow where

-- * base
import Foreign hiding (free)
import Foreign.C.Types

--------------------------------------------------------------------------------

newtype SType = SType {unSType :: CInt}

stUnsymmetric :: SType
stUnsymmetric =  SType 0

stSquareSymmetricUpper :: SType
stSquareSymmetricUpper =  SType 1

stSquareSymmetricLower :: SType
stSquareSymmetricLower =  SType (-1)


newtype XType = XType {unXType :: CInt}

xtPattern :: XType
xtPattern =  XType 0

xtReal    :: XType
xtReal    =  XType 1

xtComplex :: XType
xtComplex =  XType 2

xtZomplex :: XType
xtZomplex =  XType 3


-- defined in cholmod_cholesky:
newtype SystemType = SystemType {unSystemType :: CInt}

stA :: SystemType
stA =  SystemType 0    -- CHOLMOD_A

stLDLt :: SystemType
stLDLt =  SystemType 1 -- CHOLMOD_LDLt


newtype DoTranspose = DoTranspose {unDoTranspose :: CInt}

noTranspose :: DoTranspose
noTranspose =  DoTranspose 0

doTranspose :: DoTranspose
doTranspose =  DoTranspose 1


newtype NormType = NormType {unNormType :: CInt}
infNorm :: NormType
infNorm =  NormType 0

oneNorm :: NormType
oneNorm =  NormType 1

twoNorm :: NormType
twoNorm =  NormType 2


data Common
data Triplet
data Sparse
data Dense
data Factor

--------------------------------------------------------------------------------

foreign import ccall unsafe "cholmod_xface.h cholmodx_allocate_common"
   common_allocate :: IO (Ptr Common)

foreign import ccall unsafe "cholmod_xface.h cholmodx_free_common"
   common_free :: Ptr Common -> IO ()

foreign import ccall unsafe "cholmod_xface.h &cholmodx_free_common"
   common_free_ptr :: FunPtr (Ptr Common -> IO ())

foreign import ccall unsafe "cholmod_xface.h &cholmodx_common_finish_and_free"
        common_finish_and_free_ptr ::  FunPtr (Ptr Common -> IO ())

foreign import ccall unsafe "cholmod.h cholmod_start"
    start :: Ptr Common -> IO ()

foreign import ccall unsafe "cholmod.h cholmod_finish"
    finish :: Ptr Common -> IO ()


foreign import ccall unsafe "cholmod.h cholmod_allocate_triplet"
    triplet_allocate :: CSize -- nrow
                     -> CSize -- ncol
                     -> CSize -- nzmax, max # of nonzeros
                     -> SType
                     -> XType
                     -> Ptr Common
                     -> IO (Ptr Triplet)

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_free_xface"
    triplet_free :: Ptr Triplet -> Ptr Common -> IO CInt

foreign import ccall unsafe "cholmod_xface.h &cholmodx_triplet_free_void"
    triplet_free_ptr :: FunPtr (Ptr Common -> Ptr Triplet -> IO ())


--------------------------------------------------------------------------------

foreign import ccall unsafe "cholmod.h cholmod_triplet_to_sparse"
    triplet_to_sparse :: Ptr Triplet -> CSize -> Ptr Common -> IO (Ptr Sparse)


foreign import ccall unsafe "cholmod.h cholmod_zeros"
    zerosL :: CSize -> CSize -> XType -> Ptr Common -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod.h cholmod_ones"
    onesL :: CSize -> CSize -> XType -> Ptr Common -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod.h cholmod_eye"
    eyeL :: CSize -> CSize -> XType -> Ptr Common -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod.h cholmod_analyze"
    analyzeL :: Ptr Sparse -> Ptr Common -> IO (Ptr Factor)

foreign import ccall unsafe "cholmod.h cholmod_factorize"
    factorizeL :: Ptr Sparse -> Ptr Factor -> Ptr Common -> IO ()

foreign import ccall unsafe "cholmod.h cholmod_solve"
    solveL :: SystemType -> Ptr Factor -> Ptr Dense -> Ptr Common 
           -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod.h cholmod_copy_dense"
    copy_dense :: Ptr Dense -> Ptr Common -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod.h cholmod_sparse_to_dense"
    sparse_to_dense :: Ptr Sparse -> Ptr Common -> IO (Ptr Dense)

--------------------------------------------------------------------------------

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_nrow"
    triplet_get_nrow :: (Ptr Triplet) -> IO CSize

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_ncol"
    triplet_get_ncol :: (Ptr Triplet) -> IO CSize

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_nzmax"
    triplet_get_nzmax :: (Ptr Triplet) -> IO CSize
    
foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_nnz"
    triplet_get_nnz :: (Ptr Triplet) -> IO CSize
    
foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_row_indices"
    triplet_get_row_indices :: (Ptr Triplet) -> IO (Ptr CInt)

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_column_indices"
    triplet_get_col_indices :: (Ptr Triplet) -> IO (Ptr CInt)

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_get_x"
    triplet_get_x :: (Ptr Triplet) -> IO (Ptr CDouble)

foreign import ccall unsafe "cholmod_xface.h cholmodx_triplet_set_nnz"
    triplet_set_nnz :: Ptr Triplet -> CSize -> IO ()

--------------------------------------------------------------------------------

foreign import ccall unsafe "cholmod_xface.h cholmodx_dense_get_nrow"
    dense_get_nrow :: (Ptr Dense) -> IO CSize

foreign import ccall unsafe "cholmod_xface.h cholmodx_dense_get_ncol"
    dense_get_ncol :: (Ptr Dense) -> IO CSize

foreign import ccall unsafe "cholmod_xface.h cholmodx_dense_get_nzmax"
    dense_get_nzmax :: (Ptr Dense) -> IO CSize
    
foreign import ccall unsafe "cholmod_xface.h cholmodx_dense_get_x"
    dense_get_x :: (Ptr Dense) -> IO (Ptr CDouble)

foreign import ccall unsafe "cholmod.h cholmod_copy_dense"
    dense_copy :: Ptr Dense -> Ptr Common -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod.h cholmod_norm_dense"
    dense_norm :: Ptr Dense -> NormType -> Ptr Common -> IO CDouble

foreign import ccall unsafe "cholmod_xface.h cholmodx_dense_free_xface"
    dense_free :: Ptr Dense -> Ptr Common -> IO CInt

foreign import ccall unsafe "cholmod.h cholmod_allocate_dense"
    dense_allocate :: CSize -> CSize -> CSize -> XType -> Ptr Common
                   -> IO (Ptr Dense)

foreign import ccall unsafe "cholmod_xface.h &cholmodx_dense_free_void"
    dense_free_ptr :: FunPtr (Ptr Common -> Ptr Dense -> IO ())

--------------------------------------------------------------------------------

foreign import ccall unsafe "cholmod_xface.h cholmodx_sparse_get_nrow"
    sparse_get_nrow :: (Ptr Sparse) -> IO CSize

foreign import ccall unsafe "cholmod_xface.h cholmodx_sparse_get_ncol"
    sparse_get_ncol :: (Ptr Sparse) -> IO CSize

foreign import ccall unsafe "cholmod_xface.h cholmodx_sparse_get_nzmax"
    sparse_get_nzmax :: (Ptr Sparse) -> IO CSize
    
foreign import ccall unsafe "cholmod_xface.h cholmodx_sparse_get_x"
    sparse_get_x :: (Ptr Sparse) -> IO (Ptr CDouble)

foreign import ccall unsafe "cholmod_xface.h cholmodx_sparse_free_xface"
    sparse_free :: Ptr Sparse -> Ptr Common -> IO CInt

foreign import ccall unsafe "cholmod_xface.h &cholmodx_sparse_free_void"
    sparse_free_ptr :: FunPtr (Ptr Common -> Ptr Sparse -> IO ())

--------------------------------------------------------------------------------

foreign import ccall unsafe "cholmod_xface.h cholmodx_factor_free_xface"
    factor_free :: Ptr Factor -> Ptr Common -> IO CInt

foreign import ccall unsafe "cholmod_xface.h &cholmodx_factor_free_void"
    factor_free_ptr :: FunPtr (Ptr Common -> Ptr Factor -> IO ())



foreign import ccall unsafe "cholmod.h cholmod_sdmult"
{-
/* -------------------------------------------------------------------------- */
/* cholmod_sdmult:  Y = alpha*(A*X) + beta*Y or alpha*(A'*X) + beta*Y */
/* -------------------------------------------------------------------------- */

/* Sparse matrix times dense matrix */
-}

    sdmult :: Ptr Sparse  -- ^ A, sparse matrix to multiply
           -> DoTranspose -- ^ 
           -> Ptr CDouble -- ^ alpha[2], scale factor for A
           -> Ptr CDouble -- ^ beta[2],  scale factor for Y
           -> Ptr Dense   -- ^ X, dense matrix to multiply
           -> Ptr Dense   -- ^ Y, resulting dense matrix
           -> Ptr Common
           -> IO CInt



