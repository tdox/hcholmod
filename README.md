hcholmod
========

hcholmod is a minimal Haskell Foreign Function Interface (FFI) binding to
[CHOLMOD](http://www.cise.ufl.edu/research/sparse/cholmod), supernodal
sparse Cholesky factorization and update/downdate.


To compile
----------

To compile and run it, you need to first get the Haskell platform, either from
your package manager (e.g., `apt-get install haskell-platform`), or from
[Haskell.org](http://www.haskell.org/platform/).


You will also have to download and build CHOLMOD.  It may be easier to get all of the dependencies by downloading and buiding [SuiteSparse](http://www.suitesparse.com).


hcholmod 0.1.0.0 was built against SuiteSparse 5.8.1.

Then do the following in your shell:

    $ export SUITESPARSE_HOME=<path to directory containg SuiteSparse-5.8.1>
    $ export LD_LIBRARY_PATH=$SUITESPARSE_HOME/lib:$LD_LIBRARY_PATH
    $ git clone git://github.com/tdox/hcholmod
    $ cd hcholmod
    $ cabal configure
    $ cabal build --extra-lib-dirs=$SUITESPARSE_HOME/lib --extra-include-dirs=$SUITESPARSE_HOME/include
    $ cabal install --user

or on a Mac:

    $ brew install cmake
    $ brew install metis
    $ cabal build --extra-include-dirs=/usr/local/include/suitesparse
    $ cabal run

So far, there is only one example. 
    
Note that hcholmod has an MIT license but CHOLMOD has a GPL 2 license.
