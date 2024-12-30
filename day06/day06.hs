import Data.List
import Data.Function
import qualified Data.Map.Strict as M

data Direction = N | S | W | E
  deriving (Show, Eq)

type Position = (Integer, Integer)
data Particle = P !Position !Direction deriving (Show,Eq)

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let inputgrid = mk2Dgrid input
      start = findstart inputgrid
      lab = M.insert (getPos start) '.' inputgrid
      path1 = nub $ map getPos $ walk lab [start]
      part1 = length path1
      olabs = map (\p -> p:M.keys (M.filter (== '#') lab)) (init path1)
      part2 = length $ filter (\l -> walk2 l [start]) olabs
  print (part1,part2)
  -- traag, part2 runt compiled in 3 sec

findstart = (`P` N) . head . M.keys . M.filter (== '^')

walk2 m start = loop start
  where
    loop path@(prc@(P p d):prcs)
      | prc `elem` prcs = True
      | otherwise       =
        case next of
          Nothing -> False
          Just p' -> loop (P (stepback p' d) d':path)
      where
        next = step2 m p d
        d' = rotate d

step2 m (r,c) N = safeMaxOn fst $ filter (\(r',c') -> r' < r && c' == c) m
step2 m (r,c) S = safeMinOn fst $ filter (\(r',c') -> r' > r && c' == c) m
step2 m (r,c) E = safeMinOn snd $ filter (\(r',c') -> r' == r && c' > c) m
step2 m (r,c) W = safeMaxOn snd $ filter (\(r',c') -> r' == r && c' < c) m

stepback (r,c) N = (r+1,c)
stepback (r,c) S = (r-1,c)
stepback (r,c) E = (r,c-1)
stepback (r,c) W = (r,c+1)

safeMaxOn f = safe (maximumBy (compare `on` f))
safeMinOn f = safe (minimumBy (compare `on` f))
safe f [] = Nothing
safe f x  = Just (f x)

walk m start = loop start
  where
    loop path@((P p d):prts)
      | v == '.' = loop (P p' d:path)
      | v == '#' = loop (P (step p d') d':path)
      | v == 'F' = P p d:path
      where
        p' = step p d
        d' = rotate d
        v = M.findWithDefault 'F' p' m

rotate N = E
rotate E = S
rotate S = W
rotate W = N

step (r,c) N = (r-1, c)
step (r,c) S = (r+1, c)
step (r,c) W = (r, c-1)
step (r,c) E = (r, c+1)

getPos :: Particle -> Position
getPos (P p _) = p

getDir :: Particle -> Direction
getDir (P _ d) = d

mk2Dgrid :: [[a]] -> M.Map (Integer, Integer) a
mk2Dgrid xss = let cmax = genericLength $ head xss
                   rmax = genericLength xss
                   coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
               in M.fromList $ zip coords (concat xss)
