

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



------------------------------------------------------------
Installing Haskell CnC 
------------------------------------------------------------

  cabal install haskell-cnc

------------------------------------------------------------a
Running Haskell CnC, Method (1): Normal method.
------------------------------------------------------------

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

