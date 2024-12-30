{-# LANGUAGE TemplateHaskell #-}

import Data.List
import Data.List.Split
import Debug.Trace
import Data.Function.Memoize
import Control.Monad.State

data Trie a = Node a Bool [Trie a] deriving (Show, Eq)
deriveMemoizable ''Trie

main :: IO ()
main = do
  [patternsU,designsU] <- splitOn "\n\n" <$> readFile "input"
  let patterns  = splitOn ", " patternsU
      designs   = lines designsU
      pTrie     =  mergeTrie . map mkTrie $ patterns
      tfindT = findT pTrie
      mtfindT = memoize3 tfindT
      part2 =  sum $ map (mtfindT pTrie 0) designs
  print $ part2

mfindT = memoize4 findT

findT :: [Trie Char] -> [Trie Char] -> Int -> [Char] -> Int
findT = loop
  where
    loop tt ct r []      = r
    loop tt ct r (x:xs) = do
      let trie = filter (\t -> nodeValue t == x) ct
      case trie of
        [] -> 0
        [Node vx vb vc] ->
          if vb then
            mfindT tt vc r xs  + if null xs then 1 else mfindT tt tt r xs
          else mfindT tt vc r xs

mkTrie :: String -> Trie Char
mkTrie [x] = Node x True []
mkTrie (x:xs) = Node x False [mkTrie xs]

mergeTrie :: [Trie Char] -> [Trie Char]
mergeTrie [] = []
mergeTrie (nx@(Node x bx cx):nodes) =
  let
      (ny,nys) = partition (\n -> nodeValue n == x) nodes
  in case ny of
       [] -> nx:mergeTrie nys
       ny -> foldl mergeNode nx ny:mergeTrie nys

mergeNode :: Trie Char -> Trie Char -> Trie Char
mergeNode (Node x bx cx) (Node y by cy) = Node x (max bx by) (mergeTrie $ cx ++ cy)

nodeValue (Node x _ _) = x
nodeChildren (Node _ _ c) = c

debug :: Show a => b -> a -> b
debug = flip traceShow
