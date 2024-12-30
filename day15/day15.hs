import Data.List
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.Array
import Control.Monad.State

type Pos = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq)
type Warehouse = Array Pos Char
type WState = State Warehouse Bool

main :: IO ()
main = do
  [whouseU, instsU] <- splitOn "\n\n" <$> readFile "input"
  let whouse = mk2Darray . lines $ whouseU
      whouse2 = mk2Darray . map (concatMap widen) . lines $ whouseU
      insts  = map readDir . filter (/= '\n') $ instsU
      start  = fst . head . filter ((== '@') . snd) $ assocs whouse
      walk1  = assocs $ execState (exec start insts) whouse
      part1  = sum $ map (calcGPS . fst) $ filter ((== 'O') . snd) walk1
      start2  = fst . head . filter ((== '@') . snd) $ assocs whouse2
      walk2  = assocs $ execState (exec2 start2 insts) whouse2
      part2  = sum $ map (calcGPS . fst) $ filter ((== '[') . snd) walk2
  print (part1, part2)

exec2 :: Pos -> [Dir] -> WState
exec2 p []     = return True
exec2 p (d:ds) = do
  v <- walk2 p d
  (if v then exec2 (next p d) ds else exec2 p ds)

walk2 :: Pos -> Dir -> WState
walk2 = push2 (pure True)

push2 :: WState -> Pos ->  Dir -> WState
push2 c p d | d `elem` [L,R] = push' c p d
push2 c p d = do
  whouse <- get
  let p' = next p d
      v  = whouse ! p
      v' = whouse ! p'
  case v' of
    '.' -> move p d >> c
    '#' -> return False
    b   -> do
      check1 <- push2 (move p d >> c) p' d
      check2 <- push2 (pure True) (next2 b p') d
      if check1 && check2 then
        return True
      else do
           put whouse
           return False

push' :: WState -> Pos ->  Dir -> WState
push' c p d = do
  whouse <- get
  let p' = next p d
      v  = whouse ! p
      v' = whouse ! p'
  case v' of
    '.' -> move p d >> c
    '#' -> return False
    b   -> push2  (move p d >> c) p' d

next2 '[' (r,c) = (r,c+1)
next2 ']' (r,c) = (r,c-1)

widen :: Char -> String
widen '#' = "##"
widen 'O' = "[]"
widen '.' = ".."
widen '@' = "@."

calcGPS (r,c) = 100 * (r - 1) + c - 1

exec :: Pos -> [Dir] -> WState
exec p []     = return True
exec p (d:ds) = do
  v <- walk p d
  (if v then exec (next p d) ds else exec p ds)

walk :: Pos -> Dir -> WState
walk = push (pure True)

push :: WState -> Pos ->  Dir -> WState
push c p d = do
  whouse <- get
  let p' = next p d
      v  = whouse ! p
      v' = whouse ! p'
  case v' of
    '.' -> move p d >> c
    'O' -> push  (move p d >> c) p' d
    '#' -> return False

move :: Pos -> Dir -> WState
move p d = do
  whouse <- get
  let p' = next p d
      v  = whouse ! p
      v' = whouse ! p'
  put $ whouse // [(p,v'), (p',v)]
  return True

next (r,c) U = (r-1,c)
next (r,c) D = (r+1,c)
next (r,c) L = (r,c-1)
next (r,c) R = (r,c+1)

readDir :: Char -> Dir
readDir '^' = U
readDir '>' = R
readDir 'v' = D
readDir '<' = L
readDir  x  = error . show $ x

nbs (x,y) = [(u,v) | u <- [x-1,x,x+1], v <- [y-1,y,y+1], u /=x || v /= y]

mk2Darray :: [[e]] -> Array (Int, Int) e
mk2Darray xss = let cmax = length $ head xss
                    rmax = length xss
                    coords = [(r,c) | r <- [1..rmax], c <-[1..cmax]]
                in array ((1,1),(rmax,cmax)) $ zip coords (concat xss)

readInt :: String -> Int
readInt = read

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
