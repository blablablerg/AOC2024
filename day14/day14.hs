import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Data.Ix
import Debug.Trace
import Data.Ord
import Control.Concurrent
import Data.Maybe
import Data.Function

type Point = (Int, Int)

main :: IO ()
main = do
  input <- filter (not . null) . lines <$> readFile "input"
  let mapSize = (101,103)
      robotRgx = "(-?\\d+),(-?\\d+).+?(-?\\d+),(-?\\d+)" :: String
      robots = map ((\[x,y,vx,vy] -> ((x,y), (vx,vy))) . map readInt . drop 1) .
                concatMap (\str -> str =~ robotRgx :: [[String]]) $ input
      after100secs = flip (!!) 100 $ iterate (map (move mapSize)) robots
      part1 = product $ map (length . \q -> filter (inQuadrant q . fst) after100secs)
                            (quadrants mapSize)
      maxloop = uncurry (*) mapSize
      points = take maxloop $ map (map fst) $ iterate (map (move mapSize)) robots
      mpoint = maximumBy (compare `on` cntNbs) points
      part2 = map (mkGridPrintable mapSize . nub . map fst)
                  (iterate (map (move mapSize)) robots)
      sloop  = findshortloop maxloop points
  printMap $ mkGridPrintable mapSize mpoint
  print $ elemIndex mpoint points
  print part1
  printpart2 part2

nbs (x,y) = [(u,v) | u <- [x-1,x,x+1], v <- [y-1,y,y+1], u /=x || v /= y]

cntNbs xs = loop xs xs
  where
    loop xs []     = 0
    loop xs (y:ys) = gotNb y xs + loop xs ys

    gotNb x xs = let ns = nbs x
                 in  bool2Int $ any (`elem` xs) ns

bool2Int True = 1
bool2Int False = 0

findshortloop mx = loop 0
  where
    loop i (x:xs) | ((fromJust $ elemIndex x xs) + i + 1) >= mx = loop (i+1) xs
                  | otherwise                               = i

detectloop xs = loop 0 xs xs
  where
    loop n (x:xs) (_:y:ys) | x == y = n
                            | otherwise = loop (n+1) xs ys

printpart2 :: [[(Point, Char)]] -> IO ()
printpart2 = loop 0
  where
    loop n (p:ps) = do
      putStrLn $ "after " ++ show n ++ " seconds:"
      printMap p
      threadDelay 1_000_000
      loop (n+1) ps


move (sx, sy) ((x,y),(vx,vy)) = (((x+vx) `mod` sx, (y+vy) `mod` sy), (vx,vy))

inQuadrant ((x1,x2), (y1,y2)) (x,y)
  | ((x1,x2) `inRange` x) && ((y1,y2) `inRange` y) = True
  | otherwise                                   = False

quadrants :: (Integral a1, Integral a2) => (a1, a2) -> [((a1, a1), (a2, a2))]
quadrants (x,y) =
  let (mx,my) = ((x `div` 2) - 1, (y `div` 2) - 1)
  in  [((0,mx), (0,my)),    ((mx+2, x),(0,my)),
       ((0,mx), (my+2,y)), ((mx+2,x), (my+2,y))]

readInt :: String -> Int
readInt = read

mkGridPrintable :: (Int, Int) -> [(Int,Int)] -> [(Point, Char)]
mkGridPrintable (mx,my) points =
  let startgrid = [((x,y),'.') | y <- [0..(my-1)], x <- [0..(mx-1)], (x,y) `notElem` points]
      points'   = map (,'X') points
  in startgrid ++ points'

printMap ::  [(Point, Char)] -> IO ()
printMap m = do
  let lines = map (map snd) . groupBy (\(p1,_) (p2, _) -> (snd p1) == (snd p2))
              $ sortBy pointmapsort m
  printGrid lines

pointmapsort :: (Point, b) -> (Point, b) -> Ordering
pointmapsort = comparing (snd . fst) <> comparing (fst . fst)

pointsort :: Point -> Point -> Ordering
pointsort = comparing snd <> comparing fst

printGrid :: [String] -> IO ()
printGrid = putStrLn . unlines

debug :: Show a => b -> a -> b
debug = flip traceShow
