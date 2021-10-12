sol n = filter (\x -> goThrough x $ squares n) nums
  where
    nums = [1..n]
    squares n = takeWhile (<n) (map (^2) [1..])
    goThrough :: Int -> [Int] -> Bool
    goThrough _ [] = True
    goThrough n (x:xs)
      | n `mod` x == 0 = False
      | otherwise = goThrough n xs
