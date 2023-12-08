--1
fact :: Int -> Int
fact 0 = 1
fact n  | n < 0 = -1
        | otherwise = n * fact (n-1)

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n   | n < 0 = -1
            | otherwise = n + sumdown (n - 1)

--3
pow :: Int -> Int -> Int
pow 0 _ = 0
pow 1 _ = 1
pow _ 0 = 1
pow n m = n * pow n (m-1)
-- pow 2 3
-- =    {applying pow}
-- 2 * pow 2 2
-- =    {applying pow}
-- 2 * (2 * pow 2 1)
-- =    {applying pow}
-- 2 * (2 * 2)
-- =    {applying * }
-- 8

--4
euclid :: Int -> Int -> Int
euclid n 1 = 1
euclid n m  | r == 0    = m
            | otherwise = euclid m r
            where
                r = n `mod` m

euclid' :: Int -> Int -> Int
euclid' x y | x == y    = x
            | x < y     = euclid x (y-x)
            | y < x     = euclid (x-y) y

--5
length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' n []      = []
drop' n (x:xs)  = drop' (n-1) xs

init' :: [a] -> [a]
init' [_]       = []
init' (x:xs)    = x : init' xs

--6
and' :: [Bool] -> Bool
and' [a]     = a
and' (x:xs)  = x && and' xs

concat' :: [[a]] -> [a]
concat' []           = []
concat' ((y:ys):xs)  = y : concat' (ys : xs)
concat' ([]:xs)      = concat' xs

concat'' :: [[a]] -> [a]
concat'' []      = []
concat'' [x]     = x
concat'' (x:xs)  = x ++ concat'' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

(!!) :: [a] -> Int -> a
xs      !! 0 = head xs
(x:xs)  !! n = xs Main.!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
_ `elem'` []     = False
x `elem'` (y:ys) | x == y    = True
                | otherwise = x `elem'` ys

--7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xt) ys@(y:yt)   
    | x <= y    = x : merge xt ys
    | y < x     = y : merge xs yt

--8
msort :: Ord a => [a] -> [a]
msort []    = []
msort [a]   = [a]
msort xs = uncurry merge (foldt msort (halve xs))
    where
        halve :: [a] -> ([a], [a])
        halve xs = splitAt (length xs `div` 2) xs
        foldt :: ([a] -> [a]) -> ([a], [a]) -> ([a], [a])
        foldt f p = (f (fst p), f (snd p))
