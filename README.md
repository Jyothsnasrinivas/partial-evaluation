# Benchmarking partial evaluation in Scala, Eta, Haskell

To reproduce on your machine, run
`etlas configure --enable-uberjar-mode; etlas build` in `eta` then `sbt run` in scala, for haskell `cabal run` in `haskell`.

Results on my machine for fibonacci 5e5:

## Haskell

- native:       1.544 ms
- interpret:   10.86 ms
- partialEval: 10.47 ms

## Eta

- native:        0.649 ms
- interpret:   219.001 ms
- partialEval: 219.597 ms

## Scala

- native:       0.641 ms
- interpret:   12.572 ms
- partialEval: 11.786 ms

