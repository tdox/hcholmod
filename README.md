hcholmod
========

hcholmod is a minimal Haskell Foreign Function Interface (FFI) interface to
[CHOLMOD](http://www.cise.ufl.edu/research/sparse/cholmod), supernodal
sparse Cholesky factorization and update/downdate.


To compile
----------

To compile and run it, you need to first get the Haskell platform, either from
your package manager (e.g., `apt-get install haskell-platform`), or from
[Haskell.org](http://www.haskell.org/platform/).


You will also have to download and build CHOLMOD.  hcholmod 0.1.0.0 was built
against
CHOLMOD 2.1.2.  It may be easier to get all of the dependencies by downloading
and buiding [SuiteSparse](http://www.cise.ufl.edu/research/sparse/SuiteSparse).
hcholmod 0.1.0.0 was built against SuiteSparse 4.2.1.

Then do the following in your shell:

    $ git clone git://github.com/tdox/hcholmod
    $ cd hcholmod
    $ cabal configure
    $ cabal build
    $ cabal install --user
    
So far, there is only one example. 
    
Note that hcholmod has an MIT license but CHOLMOD has a GPL 2 license.
