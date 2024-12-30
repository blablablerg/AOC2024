import Text.Regex.PCRE

main :: IO ()
main = do
  input <- readFile "input"
  let
    rgx1  = "mul\\((\\d+),(\\d+)\\)" :: String
    rgx2  = "mul\\((\\d+),(\\d+)\\)|don't\\(\\)|do\\(\\)" :: String
    rgx3  = "don't\\(\\).*do\\(\\)" :: String
    part1 = sum $ map (\[_,x,y] -> read x * read y) (input =~ rgx1 :: [[String]])
    part2 =  sum $ map (\[_,x,y] -> read x * read y)
                 $ strip [] 1 (input =~ rgx2 :: [[String]])
  print (part1, part2)

-- Waarschijnlijk makkelijker om alle substrings van don't t/m do te verwijderen maar whatevs
strip :: (Num t, Eq t) => [[String]] -> t -> [[String]] -> [[String]]
strip y _ [] = y
strip y _ ([x,_,_]:xs) | x == "do()" = strip y 1 xs
strip y _ ([x,_,_]:xs) | x == "don't()" = strip y 0 xs
strip y 1 (x:xs) = strip (x:y) 1 xs
strip y 0 (x:xs) = strip y 0 xs


