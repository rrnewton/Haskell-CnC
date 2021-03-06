

 Intel Concurrent Collections for Haskell
 ----------------------------------------
 Author: Ryan Newton, Copyright 2009-2010


This directory contains an implementation of the Intel Concurrent
Collections programming model (CnC) for Haskell.  It works only with
GHC.

If you are looking in this directory, you are probably not using this
package through cabal.  Currently, it contains a Makefile and other
scripts that are redundant with the cabal file and will be removed in
the future.

Quick Start:
-----------------------------------------
 On Unix(ish) systems with a Bash shell, try this:

  source install_environment_vars.sh
  runcnc examples/primes.hs

You can also rerun the primes executable directly after that
(primes.exe).  To run with a particular number of threads, say 8, try:
  ./primes.exe +RTS -N8



--------------------------------------------------------------------------------
Installing Haskell CnC 
--------------------------------------------------------------------------------

You need a working installation of "ghc" and "cabal".  The easiest way
to accomplish this is with the Haskell Platform.  With those
prerequisites you should be able to install Haskell CnC directly from
the web with:

  cabal install haskell-cnc

Building from source, including installing all dependencies:

  git clone git://github.com/rrnewton/Haskell-CnC.git

  cd Haskell-CnC
  cabal update
  cabal install
  
That will cause cabal to install a number of packages from "Hackage"
and then build Haskell CnC.  If any of these dependencies break (don't
build with your version of GHC).

If you are running as root you may have an easier time with:

  cabal install --global

Otherwise make sure that ~/.cabal/bin/ is in your path.


  cabal install happy
  cabal install 

          ------------------------------------
          A GUIDE TO CABAL DEPENDENCY PROBLEMS
          ------------------------------------

Because the Haskell CnC package exists in an ecosystem of
ever-changing packages and compiler versions (cabal), and itself has
many dependencies.  Often these will get broken.  However, the
intrepid hacker can usually work around most problems.  Here, I will
try to make an account of the hacks that are needed to overcome
obstacles.


  [2011.02.14] DRBG dependency broke on hackage due to tagged-0.2
               Fixed now.
  


--------------------------------------------------------------------------------
Running Haskell CnC, Method (1): Normal method.
--------------------------------------------------------------------------------

CnC for Haskell can be used as a regular Haskell module.  
Look at "hello_world.hs" in the examples directory.


------------------------------------------------------------
Running Haskell CnC, Method (2): Inlined library.
------------------------------------------------------------

For testing purposes, Haskell CnC can inline the library and enable
the user to choose between different scheduling options and runtime
parameters statically.  Under this methodology the "runcnc" script is
used to compile and execute CnC programs.  The following environment
variable must be set:

 HASKELLCNC -- should be set to the install directory.
            (Sourcing install_environment_vars.sh is one way to
             accomplish this.)


Preprocessor variables:

 MEMOIZE    Turns on memoization of steps over tags.
            This is frequently done on a per-program basis using
            "#define MEMOIZE".

 REPEAT_PUT_ALLOWED 
            Are multiple put's into an item collection with
            the same tag valid or an error?                       

 CNC_VARIANT     Which implementation?  'pure' or 'io'?
 CNC_SCHEDULER   Which scheduler within that implementation? (1-N)
            These can also be set as environment variables when using
            runcnc.

 INCLUDEMETHOD
            ignore this, it's internal and is used for switching 
            between schedulers-as-modules or schedulers-as-includes



------------------------------------------------------------
  Testing and Benchmarking
------------------------------------------------------------

Notes on running a full benchmark suite:

To compile with profiling support (for threadscope)

  GHC_DEFAULT_FLAGS="-eventlog" NORUN=1 runcnc mandel.hs 

To run and generate a .eventlog:

  ./mandel.exe 300 300 80000 +RTS -ls -N31

Will generate mandel.exe.eventlog



