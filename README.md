# calc
Command-line calculator written in Haskell for evaluating simple mathematical expressions and solving linear equations in a single variable. Also compiled into a web version (experimental).

## Dependencies
* Haskell Stack (https://www.haskellstack.org/)
* Reflex-frp (for web version) (https://github.com/reflex-frp/reflex-platform)

## Building command line version
Change to the root directory for the project and run the following command:

```$ stack setup```

To run the automated tests:

```$ stack test```

To run the command-line calculator:

```$ stack exec calc```

To build documentation:

```$ stack haddock```

## Building the web version (experimental)
These commands need to be run from the reflex-frp environment.

For the first build, configure the package to use ghcjs.

```$ cabal configure --ghcjs```

To build the application, run:

```$ cabal build```

To run the application, open the file ```dist/build/calc/calc.jsexe/index.html``` relative to the web directory in a browser. A possible out-of-date version can be found at <https://gautham-toptal-calc.firebaseapp.com/>
