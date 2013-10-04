#!/bin/bash

# this works
ghc example1 -Wall -i../lib ../dist/build/lib/Numeric/LinearAlgebra/CHOLMOD/cholmod_xface.o -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lm -lrt -lccolamd -lcamd -llapack -lblas

# but this doesn't
# ghc -v example1 -Wall -L/usr/local/lib -lcholmod -lamd -lcolamd -lsuitesparseconfig -lmetis -lm -lrt -lccolamd -lcamd -llapack -lblas

