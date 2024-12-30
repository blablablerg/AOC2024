import Data.List
import Data.List.Split
import Data.Ix
import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Array
import Control.Monad.State
import qualified Data.PQueue.Prio.Min as PQ

type Pos = (Int, Int)
type Maze = Array Pos (Char, Int) -- (v, score)
type Q    = PQ.MinPQueue Int Pos -- score
type MState a = State (Maze, Q, (Pos, Pos), Int) a
type MState2 a = State (Maze, Q,[(Pos,(Char,Int))], (Pos, Pos)) a

main :: IO ()
main = do
  input <- map ((\[x,y] -> ((x,y),('#', maxBound :: Int))) . map readInt . splitOn ",") .
    lines <$> readFile "input"
  let start = (0,0)
      end   = (70,70)
      startinp = 1024
      maze = uncurry mk2Darray' $ end
      maze' = maze // take startinp input
      part1 = evalState walkMaze (maze',PQ.singleton 0 start, (start,end), 0)
      part2 = evalState (runMaze2) (maze',PQ.singleton 0 start, drop startinp input, (start,end))
  print part1
  print part2
  -- dag 2 opgelost met een traceM, antwoord is een na laatste input regel

runMaze2 :: MState2 Pos
runMaze2 = do
  (mz, pq,inp, (start, end)) <- get
  let (i,is) = splitAt 1 inp
  traceM $ "input: " ++ show (fst . head $ i) --output voor part2
  result <- walkMaze2
  if result then do
    put (mz // i , pq,is,(start,end))
    runMaze2
    else return $ fst . head $ i


walkMaze2 :: MState2 Bool
walkMaze2 = do
  (mz, pq,inp, (start, end)) <- get
  case PQ.getMin pq of
    Nothing -> do
      return False
    Just (st, p) ->
      if p == end then return True
      else do
        qs <- steps2 p st
        case qs of
          [] -> do
            put (mz, PQ.deleteMin pq,inp, (start,end))
            walkMaze2
          qs -> do
            let mz' = mz // map (\(q,s') -> (q,('.',s'))) qs
                pq' = foldr (uncurry PQ.insert . (\(q,s') -> (s',q))) (PQ.deleteMin pq) qs
            put (mz',pq',inp, (start, end))
            walkMaze2

walkMaze :: MState Int
walkMaze = do
  (mz, pq, (start, end),t) <- get
  case PQ.getMin pq of
    Nothing -> do
      return t
    Just (st, p) ->
      if p == end then return st
      else do
        qs <- steps p st
        case qs of
          [] -> do
            put (mz, PQ.deleteMin pq, (start,end), t)
            walkMaze
          qs -> do
            let mz' = mz // map (\(q,s') -> (q,('.',s'))) qs
                pq' = foldr (uncurry PQ.insert . (\(q,s') -> (s',q))) (PQ.deleteMin pq) qs
            put (mz',pq', (start, end), t)
            walkMaze

steps2 :: Pos -> Int -> MState2 [(Pos, Int)]
steps2 p@(x,y) s = do
  (mz, pq, _,_) <- get
  let qs =  mapMaybe (findSteps mz s) $ neighbours p
  return qs
  where
    findSteps mz s p =
      let s' = s + 1 in
      case mz !? p of
        Nothing -> Nothing
        Just ('#',vs) -> Nothing
        Just ('.',vs) -> if s' < vs then Just (p,s') else Nothing


steps :: Pos -> Int -> MState [(Pos, Int)]
steps p@(x,y) s = do
  (mz, pq, _,_) <- get
  let qs =  mapMaybe (findSteps mz s) $ neighbours p
  return qs
  where
    findSteps mz s p =
      let s' = s + 1 in
      case mz !? p of
        Nothing -> Nothing
        Just ('#',vs) -> Nothing
        Just ('.',vs) -> if s' < vs then Just (p,s') else Nothing

neighbours (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

mk2Darray' :: Int -> Int -> Array (Int, Int) (Char, Int)
mk2Darray' x y = let coords = [((x',y'), ('.', maxBound :: Int)) | y' <- [0..y], x' <-[0..x]]
                 in array ((0,0),(x,y)) $ coords

mk2Darray :: [[e]] -> Array (Int, Int) e
mk2Darray xss = let cmax = length $ head xss
                    rmax = length xss
                    coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
                in array ((1,1),(rmax,cmax)) $ zip coords (concat xss)

printArr = printMap . map (\(p,(v,s)) -> (p,v)) . assocs

printMap ::  [(Pos, Char)] -> IO ()
printMap m = do
  let lines = map (map snd) . groupBy (\(p1,_) (p2, _) -> (snd p1) == (snd p2))
              $ sortBy pointmapsort m
  printGrid lines

pointmapsort :: (Pos, b) -> (Pos, b) -> Ordering
pointmapsort = comparing (snd . fst) <> comparing (fst . fst)

pointsort :: Pos -> Pos -> Ordering
pointsort = comparing snd <> comparing fst

printGrid :: [String] -> IO ()
printGrid = putStrLn . unlines

(!?) :: Ix i => Array i a -> i -> Maybe a
(!?) arr i | inRange (bounds arr) i = Just $ arr ! i
           | otherwise              = Nothing

readInt :: String -> Int
readInt = read

debug :: Show a => b -> a -> b
debug = flip traceShow
