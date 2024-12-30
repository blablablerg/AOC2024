import Data.List
import Text.Regex.PCRE

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let part1 = sum $ map countXMAS (perms input)
      inputgrid = mk2Dgrid input
      part2 = length $ countXMAS2 inputgrid
  print $ (part1, part2)

countXMAS2 :: (Num a, Num b, Ord a, Ord b) => M.Map (a, b) Char -> [[(Char, Char)]]
countXMAS2 m = let as = filterA m
               in filter checkXMAS2 $ map (getAdj m) as

filterA :: M.Map k Char -> [k]
filterA = M.keys . M.filter (== 'A')

getAdj :: (Num a, Num b, Ord a, Ord b) => M.Map (a, b) Char -> (a, b) -> [(Char, Char)]
getAdj m (r,c)  = [(get (r-1,c-1), get (r+1,c+1)),(get (r-1,c+1), get (r+1,c-1))]
  where
    get p = M.findWithDefault 'X' p m

checkXMAS2 :: [(Char, Char)] -> Bool
checkXMAS2 [a,b] = checkSM a && checkSM b
  where
    checkSM :: (Char, Char) -> Bool
    checkSM (a,b) = (a == 'S' && b == 'M') || (a == 'M' && b == 'S')

mk2Dgrid :: [[a]] -> M.Map (Int, Int) a
mk2Dgrid xss = let cmax = length $ head xss
                   rmax = length xss
                   coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
               in M.fromList $ zip coords (concat xss)

perms :: [[b]] -> [[[b]]]
perms xss = let hr = map reverse xss
                txss = transpose xss
                vr = map reverse txss
                d1 = diagT xss
                d2 = map reverse d1
                d3 = diagT hr
                d4 = map reverse d3
            in [xss, hr, txss, vr, d1, d2, d3, d4]

countXMAS :: (Foldable t, RegexContext Regex a [[String]]) => t a -> Int
countXMAS xss = length $ concatMap countXMAS' xss
  where
    countXMAS' xs = let pat = "XMAS" :: String in xs =~ pat :: [[String]]

diagT :: [[b]] -> [[b]]
diagT = loop 1 []
  where
  loop n yss [] = yss
  loop n yss xss =
    let (target, keep) = splitAt n xss
        ys = map head target
        keep' = filter (not . null) $ map tail target
    in  loop (n+1) (ys:yss) (keep' ++ keep)

