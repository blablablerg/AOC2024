import Data.List
import Data.List.Split
import Data.Function
import Data.Bits
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Debug.Trace

type Key = (Int, Int, Int, Int)

main :: IO ()
main = do
  input <- map readInt . lines <$> readFile "input"
  let example = [1,2,3,2024]
      bananas = map (take 2000 . map (`mod` 10) . iterate evolve) input
      prices = map calcPrice bananas
      part1 = sum . map (flip (!!) 2000 . iterate evolve) $ input
      part2   = snd . maximumBy (compare `on` snd) $ runST $ mkTotals prices
  print (part1, part2)

-- mutation in Haskell
mkTotals :: [[(Key,Int)]] -> ST s [(Key,Int)]
mkTotals prices = do
  totals <- newArray bounds 0 :: ST s (STArray s Key Int)
  forM_ prices
    (\price -> do
       checks <- newArray bounds False :: ST s (STArray s Key Bool)
       forM_ price
         (\(k,v) -> do
            seen <- readArray checks k
            if seen then
              return ()
            else do
              writeArray checks k True
              cv <- readArray totals k
              writeArray totals k (cv + v)))
  getAssocs totals
  where
    bounds = ((-9,-9,-9,-9), (9,9,9,9))

calcPrice :: Num b => [b] -> [((b, b, b, b), b)]
calcPrice xs = zip (seq4 . changes $ xs) (drop 4 xs)
  where
    seq4 [a,b,c] = []
    seq4 (a:b:c:d:es) = (a,b,c,d):seq4 (b:c:d:es)
    changes xs = zipWith (-) (tail xs) xs

evolve :: Int -> Int
evolve = prune . mix (* 2048) .
         prune . mix (`div` 32) .
         prune . mix (* 64)
  where
    mix f s = xor (f s) s
    prune = flip mod 16777216

readInt :: String -> Int
readInt = read

debug :: Show a => b -> a -> b
debug = flip traceShow
