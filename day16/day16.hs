import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Data.Ix
import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Array
import Control.Monad.State
import qualified Data.PQueue.Prio.Min as PQ

type Pos = (Int, Int)
data Dir = N | S | W | E deriving (Show, Eq, Ord, Ix)
type Gate = Array Dir Int
type Particle = (Pos, Dir)
type Maze = Array Pos (Char, Int) -- (v, score)
type Maze2 = Array Pos (Char, Gate) -- (v, score)
type Q    = PQ.MinPQueue Int Particle -- score
type Q2    = PQ.MinPQueue Int [Particle]
type MState a = State (Maze, Q) a
type MState2 a = State (Maze2, Q2) a


main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let maze = mk2Darray $ map (map (, maxBound :: Int)) input
      maxbgates = let mx = maxBound :: Int
                  in  array (N,E) [(N, mx),(S, mx), (W,mx), (E,mx)]
      maze2 = mk2Darray $ map (map (, maxbgates)) input
      start = fst . head . filter ((== ('S', maxBound :: Int)) . snd) $ assocs maze
      part1 = evalState (walkMaze (maxBound :: Int)) (maze, PQ.singleton 0 (start, E))
      paths = evalState (walkMaze2 []) (maze2, PQ.singleton 0 [(start, E)])
      minscore = minimum $ map fst paths
      part2    = length . nub $ concat . map (map fst . snd) $ filter ((== minscore) . fst) paths
  print (part1, part2)

walkMaze2 :: [(Int, [Particle])] -> MState2 [(Int, [Particle])]
walkMaze2 paths = do
  (mz, pq) <- get
  case PQ.getMin pq of
    Nothing -> do
      return paths
    Just (ps, path@((p,d):rest)) -> do
      case mz ! p of
        ('E', s) -> do
          put (mz, PQ.deleteMin pq)
          walkMaze2 $ (ps, path):paths -- zit niet alleen minpaths in
        (v, gt) -> do
          qs <- steps2 (p,d) ps
          case qs of
            [] -> do
              put (mz, PQ.deleteMin pq)
              walkMaze2 paths
            qs -> do
              let mz' = foldr (\(pd,s') mz -> updatemz mz pd s') mz qs
                  pq' = foldr (uncurry PQ.insert . (\(qd,s') -> (s', qd:path)))
                              (PQ.deleteMin pq) qs
              put (mz',pq')
              walkMaze2 paths

updatemz :: Maze2 -> Particle -> Int -> Maze2
updatemz mz (p,d) s = let (v, gt) = mz ! p
                          gt' = gt // [(d,s)]
                      in mz // [(p, (v, gt'))]

steps2 :: Particle -> Int -> MState2 [(Particle, Int)]
steps2 (p,d) s = do
  (mz, pq) <- get
  return $ mapMaybe (calcScore mz (p,d) s) (adjacents (p,d))
  where
    calcScore mz (p,d) s (p',d')=
      let (v, gt) = mz ! p'
      in case v of
           '#' -> Nothing
           _   -> let s' = gt ! d'
                      s'' = s + penalty d d'
                  in if s'' <= s'
                     then Just ((p',d'), s'')
                     else Nothing

walkMaze :: Int -> MState Int
walkMaze n = do
  (mz, pq) <- get
  case PQ.getMin pq of
    Nothing -> return n
    Just (ps, (p,d)) ->
      case mz ! p of
        ('E', s) -> do
          put (mz, PQ.deleteMin pq)
          if ps < n then walkMaze ps else walkMaze n
          -- if s < n then return s else return n
        (v, s) -> do
          qs <- steps (p,d) ps
          case qs of
            [] -> do
              put (mz, PQ.deleteMin pq)
              walkMaze n
            qs -> do
              let mz' = mz // map (\((q,d), (v,s')) -> (q,(v,s'))) qs
                  pq' = foldr (uncurry PQ.insert . (\(qd, (v,s')) -> (s', qd))) (PQ.deleteMin pq) qs
              put (mz',pq')
              walkMaze n



steps :: Particle -> Int -> MState [(Particle, (Char , Int))]
steps (p,d) s = do
  (mz, pq) <- get
  let qs = map (\(q,d') -> ((q,d'), mz ! q)) (adjacents (p,d))
      qs' = map (\((q,d'), (v,s')) -> ((q,d'), (v,s'), s + penalty d d')) qs
      fqs = map (\(qd,(v,s'), x) -> (qd,(v,x))) $
            filter (\(qd, (v,s'), x) -> v /= '#' && x < s') qs'
  return fqs

penalty :: (Eq a1, Num a2) => a1 -> a1 -> a2
penalty d d' | d == d'   = 1
             | otherwise = 1001

adjacents :: Particle -> [Particle]
adjacents (p,d) = let dc = rotC d
                      dcc = rotCC d
                  in [(fwd p d, d), (fwd p dc, dc), (fwd p dcc, dcc)]

fwd :: (Num a, Num b) => (a, b) -> Dir -> (a, b)
fwd (r,c) N = (r-1,c)
fwd (r,c) S = (r+1,c)
fwd (r,c) E = (r,c+1)
fwd (r,c) W = (r,c-1)

rotC N = E
rotC E = S
rotC S = W
rotC W = N

rotCC N = W
rotCC W = S
rotCC S = E
rotCC E = N

nbs (x,y) = [(u,v) | u <- [x-1,x,x+1], v <- [y-1,y,y+1], u /=x || v /= y]

mk2Darray :: [[e]] -> Array (Int, Int) e
mk2Darray xss = let cmax = length $ head xss
                    rmax = length xss
                    coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
                in array ((1,1),(rmax,cmax)) $ zip coords (concat xss)

printMap ::  [(Pos, Char)] -> IO ()
printMap m = do
  let lines = map (map snd) . groupBy (\(p1,_) (p2, _) -> (fst p1) == (fst p2))
              $ sortBy pointmapsort m
  printGrid lines

pointmapsort :: (Pos, b) -> (Pos, b) -> Ordering
pointmapsort = comparing (fst . fst) <> comparing (snd . fst)

pointsort :: Pos -> Pos -> Ordering
pointsort = comparing snd <> comparing fst

printGrid :: [String] -> IO ()
printGrid = putStrLn . unlines

debug :: Show a => b -> a -> b
debug = flip traceShow
