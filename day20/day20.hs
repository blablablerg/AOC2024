import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Data.Ix
import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Array
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import qualified Data.PQueue.Prio.Min as PQ

type Pos = (Int, Int)
data Dir = N | S | W | E deriving (Show, Eq, Ord, Ix)
type Gate = Array Dir Int
type Particle = (Pos, Dir)
type Maze = Array Pos Char
type Q    = PQ.MinPQueue Int [Pos]
type MState a = State (Maze, Q,[Pos]) a

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let maze = mk2Darray $ input
      start = fst . head . filter ((== 'S') . snd) $ assocs maze
      path = evalState walkMaze (maze, PQ.singleton 1 [start],[])
      part1 =  length . filter (>= 100) . concat . calcCheats $ path
      part2 =  length . filter (>= 100) . concat . calcCheats2 $ path
  print (part1,part2)

distance (r1,c1) (r2,c2) = abs (r2 - r1) + abs (c2 - c1)

calcCheats2 zs = loop 1 [] zs
  where
    tl = length zs
    vs = zip zs [1..tl]
    ms = M.fromList vs
    loop _ rs [] = reverse rs
    loop l rs (x : xs) =
      let cheats =
            mapMaybe
              (\x' -> case x' `M.lookup` ms of
                       Nothing -> Nothing
                       Just v  -> Just $ v - (distance x x')) $
              filter (\x' -> let d = distance x x'
                             in  (d <= 20)) xs
          results = filter (> 0) . map (\c -> c - l) $ cheats
      in loop (l+1) (results : rs) xs

calcCheats zs = loop 1 [] zs
  where
    tl = length zs
    vs = zip zs [1..tl]
    ms = M.fromList vs
    loop _ rs [] = reverse rs
    loop l rs (x : xs) =
      let cheats = mapMaybe (`M.lookup` ms) (nb2 x)
          results = filter (> 0) . map (\c -> c - l - 2) $ cheats
      in loop (l+1) (results : rs) xs

nb2 (r, c) = [(r - 2, c), (r + 2, c), (r, c - 2), (r, c + 2)]

walkMaze :: MState [Pos]
walkMaze = do
  (mz, pq,r) <- get
  case PQ.getMin pq of
    Nothing -> return []
    Just (_, cpath@(cp:rest)) ->
      case mz ! cp of
        'E' -> do
          return $ reverse cpath
        v -> do
          let qs = steps mz cpath
          case qs of
            [] -> do
              put (mz, PQ.deleteMin pq, r)
              walkMaze
            qs -> do
              let pq' = foldr (uncurry PQ.insert . (\q -> (1,q:cpath))) (PQ.deleteMin pq) qs
              put (mz,pq', r)
              walkMaze

steps :: Maze -> [Pos] -> [Pos]
steps mz (cp: rest) = mapMaybe (findSteps mz) $ neighbours cp
  where
    findSteps mz q =
      case mz ! q of
        'S' -> Nothing
        '#' -> Nothing
        v -> if q `notElem` rest then Just q else Nothing

neighbours (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

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
