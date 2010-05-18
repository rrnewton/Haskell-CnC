

 Intel Concurrent Collections for Haskell
 ----------------------------------------
 Author: Ryan Newton, Copyright 2009-2010

This directory contains an implementation of the Intel Concurrent
Collections programming model (CnC) for Haskell.  It works only with
GHC.

Quick Start:
-----------------------------------------
 On Unix(ish) systems with a Bash shell, try this:

  source install_environment_vars.sh
  runcnc examples/primes.hs

You can also rerun the primes executable directly after that
(primes.exe).  To run with a particular number of threads, say 8, try:
  ./primes.exe +RTS -N8

If you want to poke around in the interpreter, load the system with a
shortcut in the Makefile:

  make interact

------------------------------------------------------------
Installing Haskell CnC 
------------------------------------------------------------

You will need GHC 6.12.1 along with the Haskell Platform.
(The script scaling.hs also requires the package "HSH".)


Environment 

 HASKELLCNC -- should be set to the install directory.
            (Sourcing install_environment_vars.sh is one way to
             accomplish this.)

------------------------------------------------------------a
Running Haskell CnC, Method (1): Normal method.
------------------------------------------------------------

However, this is not to say that the system could not be used as a
regular Haskell module.  Assuming the paths are set correctly, a
client program need only "import Cnc" or "import CncPure" to use the
system.  But at the point where those modules are compiled (not used)
a scheduler will need to be selected using -DCNC_SCHEDULER and the
"-cpp" option will need to be passed to ghc.


------------------------------------------------------------
Running Haskell CnC, Method (2): Inlined library.
------------------------------------------------------------

Note that the way the system is currently used is heavily dependent on
the C preprocessor to choose between different configurations at
compile time, and to ensure that we give a single file to GHC for
whole-program optimization.



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

