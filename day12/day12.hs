import Data.List
import Data.Function
import Data.Maybe
import Data.Array
import Data.Ix
import qualified Data.Map.Strict as M
import Data.Char 
import Debug.Trace

data Direction = N | S | W | E deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- mk2Darray . lines <$> readFile "input"
  let regions = getRegions input
      fences  = map getFencesR regions
      boundaries = map getBoundariesR regions
      splitbds   = map splitBoundaries boundaries
      sides = map (sum . map (sum . uncurry calcSides)) $ splitbds
      part1 = sum $ zipWith (*) (map length regions) fences
      part2 = sum $ zipWith (*) (map length regions) sides
  print (part1, part2)

splitBoundaries rg = map convert $ groupBy ((==) `on` snd) $ sortOn snd rg
  where
    convert xs = (map fst xs, snd . head $ xs)

calcSides bs N = map (countjmps . sort . map snd) $ groupBy ((==) `on` fst) $ sortOn fst bs
calcSides bs S = map (countjmps . sort . map snd) $ groupBy ((==) `on` fst) $ sortOn fst bs
calcSides bs E = map (countjmps . sort . map fst) $ groupBy ((==) `on` snd) $ sortOn snd bs
calcSides bs W = map (countjmps . sort . map fst) $ groupBy ((==) `on` snd) $ sortOn snd bs

countjmps [] = 0
countjmps [x] = 1
countjmps (x:y:ys) | y - x == 1 = countjmps (y:ys)
                   | otherwise  = 1 + countjmps (y:ys)

getBoundariesR rg = concatMap (getBoundaries rg) rg

getBoundaries rg pt = let cbs = map (getCBoundary pt) (getNbs pt)
                          bs  = map (fst pt, ) [N,E,S,W]
                      in bs \\ cbs
  where
    getNbs ((r,c),v) = let nbs = map (,v) [(r+1,c), (r-1,c), (r,c+1), (r, c-1)]
                       in nbs `intersect` rg
    getCBoundary ((r1,c1),_) ((r2,c2),_) | r1 == r2 && c2 - c1 == 1 = ((r1,c1), E)
                                         | r1 == r2 && c1 - c2 == 1 = ((r1,c1), W)
                                         | r2 - r1 == 1 && c1 == c2 = ((r1,c1), N)
                                         | r1 - r2 == 1 && c1 == c2 = ((r1,c1), S)
                                         | otherwise = error $ show (r1,c1) ++ show (r2,c2)

getFencesR rg = sum $ map (getFences rg) rg

getFences :: [((Int, Int), Char)] -> ((Int, Int), Char) -> Int
getFences rg pt = 4 - length (getNbs pt)
  where
    getNbs ((r,c),v) = let nbs = map (,v) [(r+1,c), (r-1,c), (r,c+1), (r, c-1)]
                       in nbs `intersect` rg

getRegions arr = loop [] (assocs arr)
  where
    loop rgs [] = rgs
    loop rgs (s:st) = let rg = getRegion arr s
                      in loop (rg:rgs) (st \\ rg)

getRegion :: Array (Int, Int) Char -> ((Int, Int), Char) -> [((Int, Int), Char)]
getRegion arr pt = loop [] [pt]
  where
    loop ans [] = ans
    loop ans (pt:st) =
      let pts' = go pt
      in loop (pt:ans) (foldl' (flip (:)) pts' st) --`debug` (pt,pts'++ st,ans)
      where
        go ((r,c), v) = let ptss' = mapMaybe (arr !!?) nbs
                        in  filter (\p -> snd p == v && p `notElem` ans && p `notElem` st) ptss'
         where
           nbs = [(r+1,c), (r-1,c), (r,c+1), (r, c-1)]

mk2Darray :: [[e]] -> Array (Int, Int) e
mk2Darray xss = let cmax = length $ head xss
                    rmax = length xss
                    coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
                in array ((1,1),(rmax,cmax)) $ zip coords (concat xss)

(!!?) arr i = case (arr !? i) of
                Nothing -> Nothing
                Just e  -> Just (i,e)

(!?) arr i | inRange (bounds arr) i = Just $ arr ! i
           | otherwise              = Nothing

lkDef :: Ix i => e -> Array i e -> i -> e
lkDef def arr i | inRange (bounds arr) i = arr ! i
                | otherwise              = def

digitToInt' c | c == '.' = -1
              | otherwise = digitToInt c

debug :: Show b => a -> b -> a
debug = flip traceShow
