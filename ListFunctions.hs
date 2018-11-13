count :: Int -> [Int] -> Int
count n [] = 0;
count n (x:xs) = if n == x then 1 + (count n xs) else (count n xs)


remove :: Int -> [Int] -> [Int]
remove n [] = []
remove n (x:xs) = if n == x then remove n xs else [x] ++ (remove n xs)


sublist :: Int -> Int -> [Int] -> [Int]
sublist a b (x:xs) = if b < 0 then [] else
                      if a > 0 then sublist (a-1) (b-1) xs else
                      [x] ++ sublist a (b-1) xs


reversed :: [Int] -> [Int]
reversed [] = []
reversed (x:xs) = reversed xs ++ [x]

reversed2 :: [Int] -> [Int]
reversed2 xs = reversed2' xs []


reversed2' :: [Int] -> [Int] -> [Int]
reversed2' [] a =  a
reversed2' (x:xs) a = reversed2' xs (x:a)

indexOf :: Int -> [Int] -> Int
indexOf n (x:xs) = indexOf' n (x:xs) 0

indexOf' n [] cur = -1
indexOf' n (x:xs) cur = if(n==x) then cur else
                             indexOf' n xs (cur+1)
