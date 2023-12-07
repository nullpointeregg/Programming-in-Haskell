--1
sumOfSquares :: Int
sumOfSquares = sum [x^2 | x <- [1..100]]

--2
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m] ]

--3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

--4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

--5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) |  x <- [1..n],
                        y <- [1..n],
                        z <- [1..n],
                        x^2 + y^2 == z^2]

--6
perfects :: Int -> [Int]
perfects n = [x | x <- [6..n], sum (properDivisors x) == x]

properDivisors :: Int -> [Int]
properDivisors n = [x | x <- [1..n-1], n `mod` x == 0]

--7
foo1 :: [(Int, Int)]
foo1 = [(x,y) | x <- [1,2], y <- [3,4]]
foo2 :: [(Int, Int)]
foo2 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

--8
find :: Eq a => a -> [(a,b)] -> [b]
find key table = [v | (k', v) <- table, key == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

--9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [uncurry (*) p | p <- zip xs ys]
-- scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
