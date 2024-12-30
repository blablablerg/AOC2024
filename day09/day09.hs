import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

import qualified Data.Sequence as S
import Debug.Trace

main :: IO ()
main = do
  input <- map (\x -> read x :: Int) . tail . splitOn "" . head . lines <$> readFile "input"
  let files = map (input !!) [0,2 .. length input - 1]
      spaces = map (input !!) [1,3 .. length input - 1]
      blocks = S.fromList $ mkBlocks input
      maxID = ((length input - 1) `div` 2)
      part1 = checksum $ fillBlocks blocks
      part2 = checksum $ fillBlocks2 maxID blocks
  print (part1, part2)
  -- fucking traag

checksum :: S.Seq (Maybe Int) -> Int
checksum = fst . foldl' (\(s,i) x -> ((maybe 0 id x) * i + s, i+1)) (0,0)

fillBlocks2 mx xs = loop mx xs
  where
    loop 0  xs = xs
    loop mx xs =
      let is = S.findIndicesL (Just mx ==) xs
          l  = length is
          h  = head is
          nts = findConseq l $ S.findIndicesL isNothing (S.take h xs)
      in case nts of
         Nothing -> loop (mx-1) xs
         Just i  ->
          let [lm,nothings,mid,vs,rm] = splitAts [i,i+l,h, h+l] xs
          in  loop (mx-1) $ lm S.>< vs S.>< mid S.>< nothings S.>< rm

splitAts is xs = loop [] 0 is xs
  where
    loop r _ [] xs = r ++ [xs]
    loop r c (i:is) xs =
      let (x', xs') = S.splitAt (i-c) xs
      in loop (r++[x']) i is xs'

fillBlocks :: Eq a => S.Seq (Maybe a) -> S.Seq (Maybe a)
fillBlocks xs = loop xs
  where
    loop (xs S.:|> x) | isNothing x = loop xs
    loop xs = case S.elemIndexL Nothing xs of
                Nothing -> xs
                Just i  -> let (xs' S.:|> v) = xs
                           in loop $ S.update i v xs'

mkBlocks :: Num t => [Int] -> [Maybe t]
mkBlocks xs = loop 0 0 xs
  where
    loop i n [] = []
    loop i n (x:xs) | i == 0 = replicate x (Just n) ++ loop 1 (n+1) xs
    loop i n (x:xs) | i == 1 = replicate x Nothing ++ loop 0 n xs

findConseq n [] = Nothing
findConseq n xs | isConseq n xs = Just $ head xs
                | otherwise     = findConseq n (tail xs)

isConseq 1 _ = True
isConseq n (x:y:ys) | y - x == 1 = isConseq (n-1) (y:ys)
                    | otherwise  = False
isConseq _ _ = False

debug :: Show a => b -> a -> b
debug = flip traceShow
