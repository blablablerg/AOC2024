{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.List
import Data.List.Split
import Text.Read
import Debug.Trace
import Data.Ord
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Control.Lens
import Data.Bits ( Bits(xor) )

-- deze code is een shitzooi, omdat part2 handmatig is gereverse-engineered
-- zijn veel helperfuncties om handmatig te checken welke gates verwisseld moeten worden

type WireN = String
type WireV = Maybe Bool
type Wire  = (WireN,WireV)
type Wires = M.HashMap WireN WireV
data Operator = AND | OR | XOR deriving (Show, Eq)

data Gate = MkGate { _inputs :: (WireN, WireN), _op :: Operator, _output :: WireN} deriving (Show, Eq)
makeLenses ''Gate

data GateT = GateN WireN Operator (GateT) (GateT) | Input WireN deriving (Show, Eq)
makeLenses ''GateT

main :: IO ()
main = do
  [inputsU,gatesU] <- splitOn "\n\n" <$> readFile "input"
  let inputs1 = M.fromList $ (\[c,x] -> (c, Just $ intToBool . readInt $ x)) . splitOn ": " <$> lines inputsU
      gates  = toGate . splitOn " " <$> lines gatesU
      gwires = let gws = concatMap (\g -> g._output : (\(i1,i2) -> [i1,i2]) g._inputs) gates
                in gws
      wires = M.union inputs1 (M.fromList (map (,Nothing) gwires))
      part1 = bin2dec . map (fromEnum . fromJust . snd) .
        sortOn (Down . fst) . filter ((== 'z') .  head . fst) . M.toList $ run wires gates
      zs = map fst .  sortOn fst . filter ((== 'z') .  head . fst) . M.toList $ wires
      gwires2 = map fst .  sortOn fst . filter ((`notElem` ['x','y']) .  head . fst) . M.toList $ wires
      gateTreeZ = map (mkGateT gates) zs
      zWires = setZero wires
      oWires = setOnes 44 zWires
      swap0 = gates
      swap1 = swapOutputs swap0 "z13" "vcv"
      swap2 = swapOutputs swap1 "vwp" "z19"
      swap3 = swapOutputs swap2 "z25" "mps" --rond 25 (vwb mps)
      swap4 = swapOutputs swap3 "vjv" "cqm"
      tgate = "vwp"
  let -- part2 handmatig gedaan.
      run2   = run wires swap4
      csum c =bin2dec . map (fromEnum . fromJust . snd) .
              sortOn (Down . fst) . filter ((== c) .  head . fst) .
              M.toList
      xsum   = csum 'x' $ run2
      ysum   = csum 'y' $ run2
      zsum   = csum 'z' $ run2
      part2 = intercalate "," . sort $ ["z13", "vcv","vwp", "z19", "mps", "z25", "vjv", "cqm"]
  print part2

swapOutputs gates x y = map (\(MkGate ips op o) -> case o of
                              o | o == x -> MkGate ips op y
                                | o == y -> MkGate ips op x
                                | otherwise -> MkGate ips op o
                          ) gates

--handmatig checke welke gates goed zijn
printWs zWires gates i = do
    let (xkey,ykey) = (mkKey 'x' i, mkKey 'y' i)
        zWires' = M.insert xkey (Just True) zWires
        zWires'' = M.insert ykey (Just True) zWires
        zWires''' = M.insert ykey (Just True) zWires'
    print $ "No Set:"
    print $ sortOn Down . filterTrue $  run zWires    gates
    print $ "Set " ++ xkey ++ ":"
    print $ sortOn Down . filterTrue $  run zWires'   gates
    print $ "Set " ++ ykey ++ ":"
    print $ sortOn Down . filterTrue $  run zWires''  gates
    print $ "Set " ++ xkey ++ " and "++ ykey ++ ":"
    print $ sortOn Down . filterTrue $  run zWires''' gates
  where
    filterTrue = filter (\(wn,wv) -> wv == Just True) . M.toList

mkKey x = (x:) . addZero . show

addZero [x] = '0':[x]
addZero  x   = x

setZero :: Wires -> Wires
setZero wires =
  let xys = M.map (const $ Just False) .
            M.filterWithKey (\k _ -> (head k) `elem` ['x','y']) $  wires
  in M.union xys wires

setZerotill :: Int -> Wires -> Wires
setZerotill n wires =
  let xys = M.map (const $ Just False) .
            M.filterWithKey (\k _ -> (readIntB (<= n) (tail k)) &&
                              (head k) `elem` ['x','y']) $  wires
  in M.union xys wires

setZerofrom :: Int -> Wires -> Wires
setZerofrom n wires =
  let xys = M.map (const $ Just False) .
            M.filterWithKey (\k _ -> (readIntB (>= n) (tail k)) &&
                              (head k) `elem` ['x','y']) $  wires
  in M.union xys wires

setOnes :: Int -> Wires -> Wires
setOnes n wires =
  let xys = M.map (const $ Just True) .
            M.filterWithKey (\k _ -> (readIntB (<= n) (tail k)) &&
                              (head k) `elem` ['x','y']) $  wires
  in M.union xys wires

levelsT :: GateT -> Int
levelsT (GateN _ _ g1 g2) = 1 + levelsT g1 + levelsT g2
levelsT (Input _) = 0

flattenGT :: GateT -> (WireN, Operator, [String], [String], [String])
flattenGT (GateN wire op g1 g2) =
  (wire, op, sort $ collectw2 g1 g2,
   sort $ collectx2 g1 g2, sort $ collecty2 g1 g2)

collectx2 g1 g2 = collectx g1 ++ collectx g2
collectx (GateN wire op g1 g2) = collectx2 g1 g2
collectx (Input wire) | (head wire == 'x') = [wire]
collectx (Input wire) | (head wire /= 'x') = []

collecty2 g1 g2 = collecty g1 ++ collecty g2
collecty (GateN wire op g1 g2) = collecty2 g1 g2
collecty (Input wire) | (head wire == 'y') = [wire]
collecty (Input wire) | (head wire /= 'y') = []

collectw2 g1 g2 = collectw g1 ++ collectw g2
collectw (Input wire)          = []
collectw (GateN wire op g1 g2) = [wire] ++ collectw g1 ++ collectw g2

mkGateT :: [Gate] -> WireN -> GateT
mkGateT gts wire = case find (\g -> wire == g._output) gts of
  Nothing -> Input wire
  Just (MkGate (i1,i2) op _) -> GateN wire op (mkGateT gts i1) (mkGateT gts i2)

run :: Wires -> [Gate] -> Wires
run = loop []
  where
    loop [] ws [] = ws
    loop hold ws [] = loop [] ws hold
    loop hold ws (g:gs) =
      case exec ws g of
        Nothing -> loop (g:hold) ws gs
        Just v  -> loop hold (M.insert g._output (Just v) ws) gs

exec :: Wires -> Gate -> Maybe Bool
exec ws g =
  let (w1,w2) = g._inputs
      (i1,i2) = (ws M.! w1, ws M.! w2)
  in case (i1,i2) of
       (Nothing, _) -> Nothing
       (_, Nothing) -> Nothing
       (Just v1, Just v2) -> Just $ toInst g._op v1 v2

toGate :: [String] -> Gate
toGate [a,b,c,d,e] = MkGate (a,c) (toOp b) e

toInst :: Operator -> (Bool -> Bool -> Bool)
toInst AND = (&&)
toInst OR  = (||)
toInst XOR = xor

toOp :: String -> Operator
toOp "AND" = AND
toOp "OR"  = OR
toOp "XOR"  = XOR

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc x -> 2 * acc + x) 0

intToBool :: Int -> Bool
intToBool = toEnum

readIntB p x  = maybe False p (readMaybe x)

readInt :: String -> Int
readInt = read

debug :: Show a => b -> a -> b
debug = flip traceShow
