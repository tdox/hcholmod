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

module Numeric.LinearAlgebra.CHOLMOD.CholmodXFace where

-- base
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array (withArray)

-- vector
import qualified Data.Vector.Storable.Mutable as V

-- hcholmod
import Numeric.LinearAlgebra.CHOLMOD.CholmodXFaceLow

--------------------------------------------------------------------------------

data Matrix a = Matrix {fPtr :: ForeignPtr a} 
-- a can be types Dense, Sparse, or Triplet


class MatrixC a where
   getNRow  :: Matrix a -> IO CSize
   getNCol  :: Matrix a -> IO CSize
   getNZMax :: Matrix a -> IO CSize
   getX     :: Matrix a -> IO (V.MVector s CDouble)
   mtxFree  :: ForeignPtr Common -> Matrix a -> IO CInt
--   finalizer :: FinalizerEnvPtr Common a


instance MatrixC Triplet where
    getNRow  m = withForeignPtr (fPtr m) triplet_get_nrow
    getNCol  m = withForeignPtr (fPtr m) triplet_get_ncol
    getNZMax m = withForeignPtr (fPtr m) triplet_get_nzmax
    getX     m = tripletGetX m
    
    mtxFree cfp  m = withForeignPtr (fPtr m) $ \mp -> do 
                       withForeignPtr cfp $ \cp -> do
                         triplet_free mp cp
                         
--    finalizer  = triplet_free_ptr
--    free c   m = to fix: complete free functions

instance MatrixC Dense where
    getNRow  m = withForeignPtr (fPtr m) dense_get_nrow
    getNCol  m = withForeignPtr (fPtr m) dense_get_ncol
    getNZMax m = withForeignPtr (fPtr m) dense_get_nzmax
    getX     m = denseGetX m
    
    mtxFree cfp  m = withForeignPtr (fPtr m) $ \mp -> do 
                       withForeignPtr cfp $ \cp -> do
                         dense_free mp cp
--    finalizer  = dense_free_ptr

instance MatrixC Sparse where
    getNRow  m = withForeignPtr (fPtr m) sparse_get_nrow
    getNCol  m = withForeignPtr (fPtr m) sparse_get_ncol
    getNZMax m = withForeignPtr (fPtr m) sparse_get_nzmax
    getX     m = sparseGetX m
    
    mtxFree cfp  m = withForeignPtr (fPtr m) $ \mp -> do 
                       withForeignPtr cfp $ \cp -> do
                         sparse_free mp cp

--------------------------------------------------------------------------------

allocTriplet :: CSize -> CSize -> CSize ->
                SType -> XType -> ForeignPtr Common ->
                         IO (Matrix Triplet)


allocTriplet nRow nCol nZMax sType xType cfp =
    withForeignPtr cfp $ \cp -> do
      (triplet_allocate nRow nCol nZMax sType xType cp)
      >>= newForeignPtr_
      >>= (return . Matrix)

allocCommon :: IO (ForeignPtr Common)
allocCommon = newForeignPtr common_finish_and_free_ptr =<< common_allocate

startC :: ForeignPtr Common -> IO ()
startC cfp = withForeignPtr cfp start

tripletGetRowIndices :: Matrix Triplet -> IO (V.IOVector CInt)
tripletGetRowIndices m = do
  ip <- withForeignPtr (fPtr m) (triplet_get_row_indices)     :: IO (Ptr CInt)
  ipp <- newForeignPtr_ ip                              :: IO (ForeignPtr CInt)
  nZMax <- getNZMax m                                         :: IO CSize
  return $ V.unsafeFromForeignPtr ipp 0 (fromIntegral nZMax)


tripletGetColIndices :: Matrix Triplet -> IO (V.IOVector CInt)
tripletGetColIndices m = do
  jp <- withForeignPtr (fPtr m) (triplet_get_col_indices) :: IO (Ptr CInt)
  jpp <- newForeignPtr_ jp                               :: IO (ForeignPtr CInt)
  nZMax <- getNZMax m                                    :: IO CSize
  return $ V.unsafeFromForeignPtr jpp 0 (fromIntegral nZMax)

tripletGetX :: Matrix Triplet -> IO (V.MVector s CDouble)
tripletGetX m = do
  xp <- withForeignPtr (fPtr m) triplet_get_x  :: IO (Ptr CDouble)
  xpp <- newForeignPtr_ xp                     :: IO (ForeignPtr CDouble)
  nZMax <- getNZMax m                          :: IO CSize
  return $ V.unsafeFromForeignPtr xpp 0 (fromIntegral nZMax)


tripletSetNNZ :: Matrix Triplet -> CSize -> IO ()
tripletSetNNZ m nnz = withForeignPtr (fPtr m) $ \mp -> (triplet_set_nnz mp nnz)

tripletGetNNZ :: Matrix Triplet -> IO CSize
tripletGetNNZ m = withForeignPtr (fPtr m) triplet_get_nnz


tripletToSparse :: Matrix Triplet -> ForeignPtr Common -> IO (Matrix Sparse)
tripletToSparse t cfp = do
    nnz <- tripletGetNNZ t
    withForeignPtr cfp $ \cp -> do
      withForeignPtr (fPtr t) $ \tp -> do
         sp <- triplet_to_sparse tp nnz cp :: IO (Ptr Sparse)
         sfp <- newForeignPtr_ sp :: IO (ForeignPtr Sparse)
         return (Matrix sfp)


zeros :: CSize -> CSize -> XType -> ForeignPtr Common -> IO (Matrix Dense)
zeros nRow nCol xt cfp =
  withForeignPtr cfp $ \cp -> do
    op <- zerosL nRow nCol xt cp :: IO (Ptr Dense)
    ofp<- newForeignPtr_ op :: IO (ForeignPtr Dense)
    return (Matrix ofp)

ones :: CSize -> CSize -> XType -> ForeignPtr Common -> IO (Matrix Dense)
ones nRow nCol xt cfp =
  withForeignPtr cfp $ \cp -> do
    op <- onesL nRow nCol xt cp :: IO (Ptr Dense)
    ofp<- newForeignPtr_ op :: IO (ForeignPtr Dense)
    return (Matrix ofp)

eye :: CSize -> CSize -> XType -> ForeignPtr Common -> IO (Matrix Dense)
eye nRow nCol xt cfp =
  withForeignPtr cfp $ \cp -> do
    op <- eyeL nRow nCol xt cp :: IO (Ptr Dense)
    ofp<- newForeignPtr_ op :: IO (ForeignPtr Dense)
    return (Matrix ofp)


analyze :: Matrix Sparse -> ForeignPtr Common -> IO (ForeignPtr Factor)
analyze m cfp = withForeignPtr (fPtr m) $ \mp -> do
                   withForeignPtr cfp $ \cp -> do
                     lp <- analyzeL mp cp :: IO (Ptr Factor)
                     newForeignPtrEnv factor_free_ptr cp lp

factorize :: Matrix Sparse -> ForeignPtr Factor -> ForeignPtr Common -> IO ()
factorize m ffp cfp =
    withForeignPtr (fPtr m) $ \mp -> do
      withForeignPtr ffp $ \fp -> do
        withForeignPtr cfp $ \cp ->  (factorizeL mp fp cp)
        
solve ::  SystemType
      -> ForeignPtr Factor
      -> Matrix Dense
      -> ForeignPtr Common
      -> IO (Matrix Dense)

solve st ffp m cfp =
    withForeignPtr ffp $ \fp -> do
      withForeignPtr (fPtr m) $ \mp -> do
        withForeignPtr cfp $ \cp -> do
          xp <- solveL st fp mp cp
          xfp <- newForeignPtr_ xp
          return (Matrix xfp)

denseGetX :: Matrix Dense -> IO (V.MVector s CDouble)
denseGetX m = do

  nZMax <- getNZMax m                       :: IO CSize
  xp <- withForeignPtr (fPtr m) dense_get_x :: IO (Ptr CDouble)
  xpp <- newForeignPtr_ xp                  :: IO (ForeignPtr CDouble)
  return $ V.unsafeFromForeignPtr xpp 0 (fromIntegral nZMax)



denseCopy :: Matrix Dense -> ForeignPtr Common -> IO (Matrix Dense)
denseCopy m cfp =
    withForeignPtr (fPtr m) $ \mp -> do
      withForeignPtr cfp $ \cp -> do
        m2p <- dense_copy mp cp
        m2fp <- newForeignPtr_ m2p
        return (Matrix m2fp)

sdMult :: Matrix Sparse  -- ^ A, sparse matrix to multiply
       -> DoTranspose    -- ^ 
       -> [CDouble]      -- ^ alpha[2], scale factor for A
       -> [CDouble]      -- ^ beta[2],  scale factor for Y
       -> Matrix Dense   -- ^ X, dense matrix to multiply
       -> Matrix Dense   -- ^ Y, resulting dense matrix
       -> ForeignPtr Common
       -> IO CInt

sdMult s doT alphaL betaL x y cfp =
    withForeignPtr (fPtr s) $ \sp -> do
      withArray alphaL $ \alpha -> do
        withArray betaL $ \beta -> do
          withForeignPtr (fPtr x) $ \xp -> do
            withForeignPtr (fPtr y) $ \yp -> do
              withForeignPtr cfp $ \cp -> do
                sdmult sp doT alpha beta xp yp cp


denseNorm :: Matrix Dense -> NormType -> ForeignPtr Common -> IO CDouble

denseNorm m nt cfp = 
    withForeignPtr (fPtr m) $ \mp -> do
      withForeignPtr cfp $ \cp -> do
        dense_norm mp nt cp


sparseToDense :: Matrix Sparse -> ForeignPtr Common -> IO (Matrix Dense)
sparseToDense m cfp =
    withForeignPtr (fPtr m) $ \mp -> do
      withForeignPtr cfp $ \cp -> do
        m2p <- sparse_to_dense mp cp
        m2fp <- newForeignPtr_ m2p
        return (Matrix m2fp)



sparseGetX :: Matrix Sparse -> IO (V.MVector s CDouble)
sparseGetX m = do
  xp <- withForeignPtr (fPtr m) sparse_get_x   :: IO (Ptr CDouble)
  xpp <- newForeignPtr_ xp                     :: IO (ForeignPtr CDouble)
  nZMax <- getNZMax m                          :: IO CSize
  return $ V.unsafeFromForeignPtr xpp 0 (fromIntegral nZMax)

