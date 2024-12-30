
import Data.List

main :: IO ()
main = do
  input' <- map  (map read . words) . lines <$> readFile "input"
  let input = map sort $ transpose input'
      part1 = sum $ map abs $ zipWith (-) (input!!0) (input!!1)
      part2 = sum $ map (\x -> similarity x (input!!1)) (input!!0)
  print (part1, part2)

similarity :: Int -> [Int] -> Int
similarity x ys = (*) x . length $ filter (x ==) ys
