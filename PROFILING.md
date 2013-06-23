# profiling versus eventlog
NOTE: -eventlog and -prof are mutually exlusive!

one can thus use only one of either from below:

## profiling
    cabal configure --enable-executable-profiling --ghc-options="-prof -fprof-auto-calls" && cabal build

    +RTS -h -p / +RTS -hc -p
    +RTS -hy -p
    +RTS -hd -p


## eventlog
    cabal configure --ghc-options="-eventlog -threaded -debug" && cabal build

    +RTS -lf -ls -l


# debug is awesome and works with both

    cabal configure --ghc-options="-debug" && cabal build

    +RTS -xc
