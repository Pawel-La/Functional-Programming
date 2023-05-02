import Data.List
howManyTrios = length [(x,y,z) | x <- [1..100], y <- [1..100], z <- [1..100], x^2 + y^2 == z^2]

isPrime :: Integral t => t -> Bool
isPrime n = n `elem` eratoSieve [2..] where {eratoSieve :: [Int] -> [Int]; eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]}
