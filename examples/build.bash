#!/bin/bash
ghc example1 -Wall -i../lib ../dist/build/lib/Numeric/LinearAlgebra/CHOLMOD/cholmod_xface.o -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lm -lrt -lccolamd -lcamd -llapack -lblas

# ghc example1 -Wall -i../lib ../dist/build/lib/Numeric/LinearAlgebra/CHOLMOD/cholmod_xface.o -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lccolamd -lcamd -llapack -lblas

# ghc -v example1 -Wall -i../lib -L../dist/build/lib/Numeric/LinearAlgebra/CHOLMOD -lcholmod_xface -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lccolamd -lcamd -llapack -lblas

# ghc -v example1 -Wall -i../lib -L/home/tad/code/haskell/hcholmod/dist/build/lib/Numeric/LinearAlgebra/CHOLMOD -lcholmod_xface -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lccolamd -lcamd -llapack -lblas

