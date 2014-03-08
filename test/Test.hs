module Main where

--import Data.IORef
import System.Exit
import Test.QuickCheck
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import Data.List (delete)

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- return [True]
  if and results
    then return ()
    else exitFailure



instance Arbitrary (Gr () ()) where
  arbitrary = choose (1, 100) >>= arbitraryGraph
              --sized arbitraryGraph
    where
      arbitraryGraph :: Int -> Gen (Gr () ())
      arbitraryGraph 1 = return $ insNode (1, ()) empty
      arbitraryGraph n = do
                g <- arbitraryGraph (n-1)
                let ns = nodes g
                npred <- choose (1, n`div`2)
                nsucc <- choose (1, n`div`2)
                pred  <- sampleList npred ns
                succ  <- sampleList nsucc (n:ns) -- include cycles
                return $ (toAdj pred, n, (), toAdj succ) & g
          where toAdj = map $ (,) ()

      -- | returns a random sample list of given length
      sampleList :: Int -> [a] -> Gen [a]
      sampleList n xs = sample' n [0..length xs - 1] xs

        where sample' :: Int -> [Int] -> [a] -> Gen [a]
              sample' 1 _   _ = return []
              sample' _ []  _ = return []
              sample' n ind xs = do 
                    i <- elements ind
                    let v = xs !! i
                    xs' <- sample' (n-1) (delete i ind) xs
                    return $ v : xs'
