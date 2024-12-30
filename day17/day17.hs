{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Debug.Trace
import Data.Maybe
import Control.Monad.State
import qualified Data.Vector as V
import Control.Lens
import Data.Bits
import Data.Char 

data Register = MkReg { _rA :: Int, _rB :: Int, _rC :: Int} deriving (Show, Eq)
data Memory = MkMem { _register :: Register, _program :: V.Vector Int,
                      _pointer :: Int, _output :: [Int]} deriving (Show, Eq)
makeLenses ''Register
makeLenses ''Memory

type Computer a = State Memory a

main :: IO ()
main = do
  [regsU,programU] <- splitOn "\n\n" <$> readFile "input"
  let regRgx    = "(\\d+)" :: String
      progRgx   = "Program: (.+)" :: String
      registers = mkRegister . map readInt . concatMap (drop 1) .
                  concatMap (\str -> str =~ regRgx :: [[String]]) $ lines regsU
      program   = V.fromList . map readInt . splitOn "," . concat . drop 1 . concat .
                  (\str -> str =~ progRgx :: [[String]]) $ programU
      input     = V.toList program
      part1     = evalState run (MkMem registers program 0 [])
      part2     = minimum . map bin2dec $ foldOptions input
  print (part1, part2)

replaceV :: [Char] -> [Char]
replaceV [] = []
replaceV (x:xs) | x == 'v' = '0':replaceV xs
                | otherwise = x:replaceV xs

bin2dec :: String -> Int
bin2dec = foldl (\acc x -> 2 * acc + digitToInt x) 0

options xs =
  let os = map getOptions xs
      l  = length xs
      l3  = 3 * length xs
      vs = take (length xs) makeVs
      os' = zipWith (curry (\(o,v) -> zipWith (++) o (replicate (length o) v))) os vs
  in map (map (addVs l3) . filter (\o -> length o <= l3). map (eatZeros l3)) os'

addVs l xs
  | length xs < l = let d = l - length xs in replicate d 'v' ++ xs
  | otherwise = xs

makeVs = "": map (++ "vvv") makeVs

foldOptions zs =
  let (y:ys) = options zs
  in loop y ys
  where
    loop r []      = r
    loop r (x:xs) = loop (getOptions3 x r) xs


getOptions3 x1 x2 =
  let o1 = x1
      o2 = x2
      vs = [zip x y | x <- o1, y <- o2]
  in mapMaybe canMerge vs

getOptions2 x1 x2 =
  let o1 = map (++ "vvv") x1
      o2 = x2
      vs = [uncurry zip . equalize $ (x,y) | x <- o1, y <- o2]
  in mapMaybe canMerge vs

equalize (s1,s2) | length s1 == length s2 = (s1, s2)
equalize (s1,s2) | length s1 > length s2 =
                   let d = length s1 - length s2
                   in (s1, replicate d 'v' ++ s2)
equalize (s1,s2) | length s1 < length s2 =
                   let d = length s2 - length s1
                   in (replicate d 'v' ++ s1, s2)

eatZeros :: Int -> [Char] -> [Char]
eatZeros l []  = []
eatZeros l s@(x:xs)
  | (length s) <= l = x:eatZeros l xs
  | otherwise = if x `elem` ['0','v']  then eatZeros l xs
                else x:eatZeros l xs


getOptions x = mapMaybe (getOption x) [0..7]

getOption x b =
  let b1 = b `xor` 1
      d  = b `xor` 2
      x2 = x `xor` b1
      s1 = replicate d 'v' ++ dec2bin b
      s2 = dec2bin x2 ++ replicate d 'v'
  in canMerge $ zip s1 s2 --`debug` (s1, s2)

canMerge = traverse id . map merge
  where
    merge (x,y) | x == y = Just x
    merge (x,'v')        = Just x
    merge ('v',y)        = Just y
    merge _              = Nothing

dec2bin :: Int -> String
dec2bin = fillzeros . reverse .
          unfoldr (\x -> if x == 0 then Nothing
                         else Just (intToDigit $ x `rem` 2, x `div` 2))

fillzeros xs = replicate (3 - (length xs)) '0' ++ xs

run :: Computer [Int]
run = do
  p <- gets _pointer
  pg <- gets _program
  o <- gets _output
  if p > (V.length pg - 1) then
    return $ reverse o
  else do
    exec
    run

exec :: Computer ()
exec = do
  p <- gets _pointer
  pg <- gets _program
  let (op, opn) = (pg V.! p, pg V.! (p + 1))
  case op of
    0 -> adv opn
    1 -> bxl opn
    2 -> bst opn
    3 -> jnz opn
    4 -> bxc opn
    5 -> out opn
    6 -> bdv opn
    7 -> cdv opn
  r <- gets _register
  -- traceM $ show r
  modify (pointer %~ (+ 2))
  return ()

adv :: Int -> Computer ()
adv x = do
  num <- getReg rA
  denom <- combo x
  setReg rA $ num `div` (2^denom)
  return ()

bxl :: Int -> Computer ()
bxl x = do
  b <- getReg rB
  setReg rB $ b `xor` x
  return ()

bst :: Int -> Computer ()
bst x = do
  v <- combo x
  setReg rB $ v `mod` 8
  return ()

jnz :: Int -> Computer ()
jnz x = do
  a <- getReg rA
  if a == 0 then return ()
  else
    modify $ pointer .~ (x - 2)
  return ()

bxc :: Int -> Computer ()
bxc x = do
  b <- getReg rB
  c <- getReg rC
  setReg rB $ b `xor` c
  return ()

out :: Int -> Computer ()
out x = do
  o <- gets _output
  v <- combo x
  modify $ output .~ ((v `mod` 8):o)
  return ()

bdv :: Int -> Computer ()
bdv x = do
  num <- getReg rA
  denom <- combo x
  setReg rB $ num `div` (2^denom)
  return ()

cdv :: Int -> Computer ()
cdv x = do
  num <- getReg rA
  denom <- combo x
  setReg rC $ num `div` (2^denom)
  return ()

setReg :: Lens' Register Int -> Int -> Computer ()
setReg l x = modify (register . l .~ x)

getReg :: Lens' Register Int -> Computer Int
getReg r = gets (view r . _register)

combo :: Int -> Computer Int
combo x | x `elem` [0..3] = pure x
combo 4                   = getReg rA
combo 5                   = getReg rB
combo 6                   = getReg rC

mkRegister :: [Int] -> Register
mkRegister [a,b,c] = MkReg a b c

readInt :: String -> Int
readInt = read

debug :: Show a => b -> a -> b
debug = flip traceShow
