import Data.List
import Data.List.Split
import Text.Regex.PCRE

main :: IO ()
main = do
  input <- filter (not . null) . map lines . splitOn "\n\n" <$> readFile "input"
  let buttonrgx = "([\\-|\\+]?\\d+).+?([\\-|\\+]?\\d+)" :: String
      inputP = map (map (map readInt . drop 1) .
                    concatMap (\str -> str =~ buttonrgx :: [[String]])) input
      invs = map (\[v1,v2,b] -> invert v1 v2) inputP
      targets = map (!! 2) inputP
      sols = map mIsInt $ zipWith mm invs targets
      sols2 = map mIsInt $ zipWith mm invs (map (map (+ 10000000000000)) targets)
      part1 = sum $ map (\[x1,x2] -> x1 * 3 + x2) sols
      part2 = sum $ map (\[x1,x2] -> x1 * 3 + x2) sols2
  print (part1, part2)

mIsInt [x1,x2] | isInt x1 && isInt x2 = [round x1, round x2]
               | otherwise            = [0,0]

isInt x = x == fromInteger (round x) --is niet perfect

mm :: (Fractional b, Integral a1, Integral a2) => (a2, [[a1]]) -> [a1] -> [b]
mm (det, [[x1,y1], [x2,y2]]) [v1, v2] =
  let det' = fromIntegral det
      m'   = map fromIntegral [x1*v1+x2*v2 ,y1*v1+y2*v2]
  in  map (/ det') m'

-- x1 = a, y1 = c, x2 = b, y2 = d
invert :: [Int] -> [Int] -> (Int, [[Int]])
invert [x1, y1] [x2, y2]
  | det /= 0 =
      let [x1',x2',y1',y2'] = map fromIntegral [x1,x2,y1,y2]
      in  (det , [[y2', - y1'], [- x2', x1']])
  | otherwise = error "singular matrix"
  where
    det = fromIntegral $ (x1 * y2) - (y1 * x2)

readInt :: String -> Int
readInt (x:xs) | x == '+'  = read xs
               | otherwise = read (x:xs)




