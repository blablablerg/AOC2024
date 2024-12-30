{-# LANGUAGE TemplateHaskell#-}

import Data.List
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Function
import Data.Array
import qualified Data.Map as M
import qualified Data.MultiSet as MS
import Control.Monad
import Control.Monad.State
import qualified Data.PQueue.Prio.Min as PQ
import Control.Lens
import Data.Function.Memoize

type Pos = (Int, Int)
type Grid = M.Map Pos GridV
data GridV  = MkGridV { _adj :: [(Pos,Char)], _visited :: Bool, _score :: Int}
makeLenses ''GridV
type Q    = PQ.MinPQueue Int (Pos,String)
type GState a = State (Grid, Q, Pos,[String]) a

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let
    digits = map ((\x -> read x :: Int) . init) input
    nf = foldl1 merge . map (uncurry nWalkm2) . toPos nPad . ('A':)
    df = concatMap (head . groupBy ((==) `on` length) . sortOn length .
                      foldl1 merge . map (uncurry dWalkm2) . toPos dPad . ('A':))
    part1f = map (minimumBy (compare `on` length) . df . df . nf)
    part1  = sum $ zipWith (*) digits (map length (part1f input))
    part2  = sum $ zipWith (*) digits $ map (msl . applyN 25 mdf . mnf) input
  print (part1, part2)

msl :: MS.MultiSet String -> Int
msl = MS.foldOccur (\xs o acc -> (length xs * o) + acc) 0

mnf = splitA . concatMap (uncurry nWalkm) . toPos nPad . ('A':)
mdf = MS.unionsMap $ splitA . concatMap (uncurry dWalkm) . toPos dPad . ('A':)

splitA = MS.fromList . map (++ "A") . splitOn "A" . init

applyN 1 f = f
applyN n f = applyN (n-1) f . f

dPad :: M.Map Char Pos
dPad = M.fromList
       [('^',(0,1)),('A',(0,2)),('<',(1,0)),('v',(1,1)),('>',(1,2))]

nPad :: M.Map Char Pos
nPad = M.fromList
       [('7',(0,0)),('8',(0,1)),('9',(0,2)),('4',(1,0)),('5',(1,1)),
        ('6',(1,2)),('1',(2,0)),('2',(2,1)),('3',(2,2)),('0',(3,1)),
        ('A',(3,2))]

toPos :: Ord a => M.Map a b -> [a] -> [(b, b)]
toPos m xs = mkSeq $ map (m M.!) xs

mkSeq :: [b] -> [(b, b)]
mkSeq xs = zip (init xs) (tail xs)

mkGrid xs = let adjs =  map (filter (\(p,v) -> p `elem` xs) . neighbours) xs
                gridvs = map (\as -> MkGridV as False (maxBound :: Int)) adjs
            in  M.fromList $ zip xs gridvs

nGrid = mkGrid $ M.elems nPad
dGrid = mkGrid $ M.elems dPad

neighbours (r,c) = [((r+1,c),'v'), ((r,c-1), '<'), ((r-1,c), '^'),((r,c+1),'>')]

setVisited gr p = M.update (\gv -> Just $ gv & visited .~ True) p gr
setScore p s = M.update (\gv -> Just $ gv & score .~ s) p

-- memoizen was niet nodig maar ok 
nWalkm = memoize2 nWalk
dWalkm = memoize2 dWalk

nWalkm2 = memoize2 nWalk2
dWalkm2 = memoize2 dWalk2

nWalk2 = walkGrid2 nGrid
dWalk2 = walkGrid2 dGrid

nWalk = walkGrid nGrid
dWalk = walkGrid dGrid

turns [] = 0
turns [x] = 0
turns (x:y:ys) | x == y = turns (y:ys)
               | otherwise = 1 + turns (y:ys)

-- order priority gevonden met trial en error
cmpDpad (x:xs) =
  case x of
    'A' -> 0
    '>' -> 4
    '<' -> 1
    '^' -> 3
    'v' -> 2

merge xs ys = [x++y| x<-xs, y<-ys]

walkGrid2 :: Grid -> Pos -> Pos -> [[Char]]
walkGrid2 gr start end =
  head . groupBy ((==) `on` turns) . sortOn turns $
  evalState walkGrid' (gr,PQ.singleton 0 (start,[]), end, [])

walkGrid gr start end =
  head . sortOn cmpDpad . head .
  groupBy ((==) `on` turns) . sortOn turns $
  evalState walkGrid' (gr,PQ.singleton 0 (start,[]), end, [])

walkGrid' :: GState [String]
walkGrid' = do
  (gr, pq,end,ans) <- get
  case PQ.getMin pq of
    Nothing -> return ans
    Just (score, (p,vs)) ->
      if p == end then do
        let path = reverse $ 'A':vs
        put (gr,PQ.deleteMin pq, end, path:ans)
        walkGrid'
      else do
        let gr'   = setVisited gr p
            qs    = steps gr' p score
        case qs of
          [] -> do
            put (gr', PQ.deleteMin pq, end,ans)
            walkGrid'
          qs -> do
            let pq' = foldr (uncurry PQ.insert .
                             (\(q,v,s') -> (s',(q,v:vs)))) (PQ.deleteMin pq) qs
                gr'' = foldr (uncurry setScore .
                              (\(q,v,s') -> (q, s'))) gr' qs
            put (gr'',pq', end, ans)
            walkGrid'

steps :: Grid -> Pos -> Int -> [(Pos,Char, Int)]
steps gr p s = mapMaybe findSteps $ _adj $ gr M.! p
  where
    -- s' = s + 1 -- step function
    findSteps (q,v) =
      let s' = s + 1
      in case gr M.! q of
         (MkGridV _ False gs) | s' <= gs -> Just (q,v,s')
                              | otherwise -> Nothing
         (MkGridV _ True _) -> Nothing

mk2Darray :: [[e]] -> Array (Int, Int) e
mk2Darray xss =
  let cmax = length $ head xss
      rmax = length xss
      coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
  in array ((1,1),(rmax,cmax)) $ zip coords (concat xss)

printMap ::  [(Pos, Char)] -> IO ()
printMap m = do
  let lines =
        map (map snd) . groupBy (\(p1,_) (p2, _) -> (fst p1) == (fst p2)) $
        sortBy pointmapsort m
  printGrid lines

pointmapsort :: (Pos, b) -> (Pos, b) -> Ordering
pointmapsort = comparing (fst . fst) <> comparing (snd . fst)

pointsort :: Pos -> Pos -> Ordering
pointsort = comparing snd <> comparing fst

printGrid :: [String] -> IO ()
printGrid = putStrLn . unlines

debug :: Show a => b -> a -> b
debug = flip traceShow
