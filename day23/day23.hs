import Data.List
import Data.List.Split
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type Comp = String
type CompSet = HS.HashSet Comp
type ConnSet = HS.HashSet CompSet
type Adjs    = HM.HashMap Comp CompSet

main :: IO ()
main = do
  input   <- map (splitOn "-") . lines <$> readFile "input"
  let conns  = mkConns input
      comps  = HS.unions $ HS.toList conns
      adjs   = mkAdj comps conns
      part1  = HS.size . HS.filter (any ((== 't'). head) . HS.toList) $
               getConns adjs conns
      part2  =  map (intercalate "," . sort . HS.toList) . HS.toList $
                getLastResult (getConns adjs) conns
  print (part1, part2)

getLastResult f x =
  let x' = f x
  in if HS.null x' then x else getLastResult f x'

getConns :: Adjs -> ConnSet -> ConnSet
getConns adjs conns = hsUnions $ HS.map (getConn adjs) conns

getConn :: Adjs -> CompSet -> ConnSet
getConn as cs = let adjs = HS.toList $ HS.map (as HM.!) cs
                    commons = foldr1 HS.intersection adjs
                in  HS.map (`HS.insert` cs) commons

mkAdj :: CompSet -> ConnSet -> Adjs
mkAdj comps conns = HM.fromList . HS.toList $
  HS.map (\c -> (c,) . HS.delete c . hsUnions $
           HS.filter (\cn -> c `HS.member` cn) conns) comps

hsUnions :: Eq a => HS.HashSet (HS.HashSet a) -> HS.HashSet a
hsUnions = HS.unions . HS.toList

mkConns = HS.fromList . map HS.fromList
