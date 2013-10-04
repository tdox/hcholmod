#!/bin/bash

# this works
ghc example1 -v -Wall -i../lib ../dist/build/lib/Numeric/LinearAlgebra/CHOLMOD/cholmod_xface.o -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lm -lrt -lccolamd -lcamd -llapack -lblas

# but this doesn't
# ghc example1 -Wall -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lm -lrt -lccolamd -lcamd -llapack -lblas

