import Data.List
import Data.Function

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let antennas = filter (\(p,f) -> f /= '.') $ mk2Dgrid input
      cmax = length $ head input
      rmax = length input
      antennaPairs = map mkPairs $ groupBy ((==) `on` snd) $ sortOn snd antennas
      antinodes = concatMap (concatMap (filter (inBoundary rmax cmax) .
                                        getAntinodes)) antennaPairs
      antinodes2 = concatMap (concatMap (getAntinodes2 rmax cmax)) antennaPairs
      part1 = length . nub $ map fst antinodes
      part2 = length . nub $ map fst antinodes2
  print (part1, part2)

inBoundary rmax cmax ((r,c),v)
  | between r 1 rmax && between c 1 cmax = True
  | otherwise = False
  where
    between x xmin xmax = xmin <= x && x <= xmax

getAntinodes2 rmax cmax [((r1,c1),v1), ((r2,c2), v2)] =
  let pA = takeWhile (inBoundary rmax cmax) $
        map (\i -> ((r1 + rd*i, c1 + cd*i), v1)) [0..]
      nA = takeWhile (inBoundary rmax cmax) $
        map (\i -> ((r1 - (rd*i), c1 - (cd*i)),v1)) [0..]
  in pA ++ nA
  where
    rd = r2 - r1
    cd = c2 - c1

getAntinodes [((r1,c1),v1), ((r2,c2), v2)] =
  let rd = r2 - r1
      cd = c2 - c1
  in  [((r1-rd, c1-cd), v1), ((r2+rd, c2+cd), v1)]

mkPairs [] = []
mkPairs (x:xs) = map (\x' -> [x,x']) xs ++ mkPairs xs

mk2Dgrid xss = let cmax = length $ head xss
                   rmax = length xss
                   coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
               in zip coords (concat xss)
