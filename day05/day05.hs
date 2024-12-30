import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Data.Function

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let rules = map ((\[x,y] -> both (\i -> read i :: Int) (y,x)) . splitOn "|")
              $ filter (elem '|') input
      rsMap = M.fromList . map collect . groupBy ((==) `on` fst) .
              sortOn fst $ rules
      updates = map (map (\x -> read x :: Int) . splitOn ",") $
                filter (\i -> (not . null) i && notElem '|' i) input
      incorrect = filter (not . checkUpdate rsMap) updates
      part1 = sum $ map getMiddle $ filter (checkUpdate rsMap) updates
      part2 = sum $ map (getMiddle . fixUpdate rsMap []) incorrect
  print (part1, part2)

getMiddle :: [a] -> a
getMiddle xs = xs !! (flip (-) 1. (`div` 2) . (+1) $ length xs)

fixUpdate :: Ord a => M.Map a [a] -> [a] -> [a] -> [a]
fixUpdate m upd [] = upd
fixUpdate m upd (u:us) =
  let afters = M.findWithDefault [] u m
  in case afters `intersect` us of
    [] -> fixUpdate m (upd ++ [u]) us
    rs -> fixUpdate m upd (rs ++ [u] ++ (us \\ rs))

checkUpdate :: Ord a => M.Map a [a] -> [a] -> Bool
checkUpdate m [] = True
checkUpdate m (u:us) =
  let afters = M.findWithDefault [] u m
  in case afters `intersect` us of
     [] -> checkUpdate m us
     rs -> False

both :: (t -> b) -> (t, t) -> (b, b)
both f (x,y) = (f x, f y)

collect :: [(a, b)] -> (a, [b])
collect rs = let key = fst . head $ rs
             in (key, map snd rs)
