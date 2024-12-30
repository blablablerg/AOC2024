import Data.List.Split
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let cals = map ((\[x,y] -> (read x :: Int, (map (\y' -> read y' :: Int). words) y)) .
                  splitOn ":") input
      part1 = sum $ map head $ filter (not . null) $ map getOps cals
      part2 = sum $ map head $ filter (not . null) $ map getOps2 cals
  print (part1, part2)
  -- niet het snelste

getOps (ans, [x]) | ans == x = [ans]
getOps (ans, [x]) | ans /= x = []
getOps (ans, x:y:ys) = do
  op<- [(*), (+)]
  getOps (ans, op x y:ys)

getOps2 (ans, [x]) | ans == x = [ans]
getOps2 (ans, [x]) | ans /= x = []
getOps2 (ans, x:y:ys) = do
  op<- [(*), (+), concInt]
  getOps2 (ans, op x y:ys)

concInt x y = read (show x ++ show y) :: Int
