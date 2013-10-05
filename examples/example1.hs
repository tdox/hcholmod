--------------------------------------------------------------------------------
--
--  Copyright (c) 2010 - 2013 Tad Doxsee
--  All rights reserved.
--
--  Author: Tad Doxsee
--
--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE  FlexibleInstances    #-}

-- * base
import Foreign           (ForeignPtr, Storable)
import Foreign.C.Types   (CDouble, CInt, CSize)

-- * vector
import qualified Data.Vector.Storable.Mutable as V

-- * cholmod
import Numeric.LinearAlgebra.CHOLMOD.CholmodXFaceLow
import Numeric.LinearAlgebra.CHOLMOD.CholmodXFace

--------------------------------------------------------------------------------

main :: IO ()
main = do

  c <- allocCommon :: IO (ForeignPtr Common)
  startC c 

  let n = 5                  :: CSize
      nz = (n+1) * n `div` 2 :: CSize
      nn = fromIntegral n    :: CSize


  at <-allocTriplet n n nz stSquareSymmetricLower xtReal c
       :: IO (Matrix Triplet)

  atNRow  <- getNRow  at :: IO CSize
  atNCol  <- getNCol  at :: IO CSize
  atNZMax <- getNZMax at :: IO CSize

  putStrLn ""
  putStrLn $ "at: (" ++ show atNRow
                     ++ " x " ++ show atNCol ++ ")"
  putStrLn $ "nzmax: " ++ show atNZMax

  iv <- tripletGetRowIndices at :: IO (V.IOVector CInt)
  jv <- tripletGetColIndices at :: IO (V.IOVector CInt)
  xt  <- tripletGetX at


  let nni = fromIntegral nn

  let ij      = [(i, j) | i <- [0 .. nni-1], j <- [0 .. i]] :: [(CInt, CInt)]
      (ia,ja) = unzip ij                                    :: ([CInt], [CInt])
      
      -- the elements of the matrix are (lower half)
      --
      --  11.0
      --  21.0  22.0
      --  31.0  32.0  33.0
      --  41.0  42.0  43.0  44.0
      --  51.0  52.0  53.0  54.0  55.0
      
      xp = [11, 21, 22, 31, 32, 33, 41, 42, 43, 44, 51, 52, 53, 54, 55]
           :: [CDouble]

  writev iv ia
  writev jv ja
  writev xt xp

  let nnz = fromIntegral $ Prelude.length xp :: CSize

  tripletSetNNZ at nnz


  as <- tripletToSparse at c :: IO (Matrix Sparse)
  b  <- ones n 1 xtReal c    :: IO (Matrix Dense)
  l  <- analyze as c         :: IO (ForeignPtr Factor)
  factorize as l c
  x  <- solve stA l b c      :: IO (Matrix Dense)

  xNRow <- getNRow x
  xNCol <- getNCol x

  xv <- getX x :: IO (V.IOVector CDouble)

  putStrLn ""
  putStrLn $ "xv: (" ++ show xNRow
                     ++ " x " ++ show xNCol ++ ")"

  xl <- readv xv
  mapM_ (putStrLn . show) xl


  r <- denseCopy b c

  let oneL = [1, 0] :: [CDouble]
      m1L = [-1, 0] :: [CDouble]

  _ <- sdMult as noTranspose m1L oneL x r c

  norm <- denseNorm r infNorm c :: IO CDouble

  putStrLn $ "norm: " ++ show norm

  putStrLn "done"



writev :: (Storable a) => V.IOVector a -> [a] -> IO ()

writev v xs =
  sequence_ [V.write v i x | (i, x) <- zip [0 .. (Prelude.length xs - 1)] xs]


readv :: (Storable a) => V.IOVector a -> IO [a]
readv v = sequence [V.read v i | i <- [0 .. (V.length v) - 1]]
