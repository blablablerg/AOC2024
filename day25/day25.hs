import Data.List
import Data.List.Split

main :: IO ()
main = do
  (locksU, keysU) <- partition (all (== '#') . head) .
                     map lines . splitOn "\n\n" <$> readFile "input"
  let keys  = map (map (subtract 1 . count (== '#')) . transpose) keysU
      locks = map (map (subtract 1 . count (== '#')) . transpose) locksU
      part1 = length $ bruteforce keys locks
  print part1

bruteforce keys locks =
  [(k,l) | k <- keys, l <- locks, all (<= 5) (zipWith (+) k l)]

count p = length . filter p
