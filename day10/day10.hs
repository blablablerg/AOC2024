import Data.List
import Data.Array
import Data.Char 
import Control.Monad

main :: IO ()
main = do
  input <- mk2Darray . map (map digitToInt') . lines <$> readFile "input"
  let starts = filter (\(i,e) -> e == 0) $ assocs input
      part1 = sum $ map (length . nub . map (\p -> [head p, last p]) . hike input) starts
      part2 = sum $ map (length . hike input) starts
  print (part1, part2)

hike arr start = loop [] start
  where
    loop path (pos, 9) = [pos:path]
    loop path (pos,h) = do
      pos' <- step pos
      case arr !? pos' of
        Nothing -> []
        Just h' -> do
          guard (h' - h == 1)
          loop (pos:path) (pos',h')

step (r,c) = [(r-1,c), (r+1,c), (r, c-1),(r,c+1)]

mk2Darray xss =
  let cmax = length $ head xss
      rmax = length xss
      coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
  in array ((1,1),(rmax,cmax)) $ zip coords (concat xss)

(!?) arr i | inRange (bounds arr) i = Just $ arr ! i
           | otherwise              = Nothing

lkDef :: Ix i => e -> Array i e -> i -> e
lkDef def arr i | inRange (bounds arr) i = arr ! i
                | otherwise              = def

digitToInt' c | c == '.' = -1
              | otherwise = digitToInt c
