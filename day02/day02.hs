main :: IO ()
main = do
    input <- map (map (\x -> read x :: Int) . words) . lines <$> readFile "input"
    let unsafe = filter (not . safe . diffs) input
        part1 = length $ filter safe $ map diffs input
        part2 = (+ part1) $ length $ filter (any (safe . diffs)) $ map dampener unsafe
    print (part1, part2)

diffs xs = zipWith (-) (tail xs) (init xs)

safe :: (Foldable t, Ord a, Num a) => t a -> Bool
safe xs = (all (<= 0) xs || all (>= 0) xs) &&
  all (\x -> let x' = abs x in x' > 0 && x' < 4) xs

dampener xss = map (`remove` xss) [1.. length xss]
remove i xs = init a ++ b
  where (a,b) = splitAt i xs
