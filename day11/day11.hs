import qualified Data.IntMap.Strict as M

main :: IO ()
main = do
  input <- map strToInt . words <$> readFile "input"
  let inputMS = M.fromList (map (,1) input)
      part1 = length . flip (!!) 25 $ iterate blink input
      part2 = sum . map snd . M.toList . flip (!!) 75 $ iterate blinkMS inputMS
  print (part1, part2)

blinkMS :: Num a => M.IntMap a -> M.IntMap a
blinkMS =  M.fromListWith (+) . concatMap changeMS . M.toList

changeMS :: (Int, t) -> [(Int, t)]
changeMS (x,o) = map (,o) $ change x

blink :: [Int] -> [Int]
blink = concatMap change

change :: Int -> [Int]
change 0 = [1]
change x =
  let x' = show x
      l  = length x'
      half = l `div` 2
  in if even l then map read [take half x', drop half x']
     else [x * 2024]

strToInt :: String -> Int
strToInt = read
