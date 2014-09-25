fglext
======

This project aims to extend Martin Erwig's [FGL Haskell library](http://hackage.haskell.org/package/fgl), by introducing new algorithm to determine the maximum flow - `Data.Graph.Inductive.Query.PreflowPush`. Moreover, since the library lacks testing, proper tests were made on random generated graphs by using `QuickCheck` and instantiating the `Arbitrary`
class for multigraphs with no labels on nodes or edges. The test implementation file is in `test/Test.hs`.

