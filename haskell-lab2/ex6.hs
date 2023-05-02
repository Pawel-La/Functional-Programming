{-# LANGUAGE BangPatterns #-}

prod' :: Num a => [a] -> a 
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' el [] = False
elem' el (x:xs) = (el == x || elem' el xs)

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = x^2 : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = 
  if x `mod` 2 == 0 then x : selectEven xs 
  else selectEven xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
  where loop acc [] = acc
        loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where loop acc [] = acc
        loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
  where loop acc [] = acc
        loop acc (x:xs) = loop (x*acc) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc+1) xs

prod'3 :: Num a => [a] -> a
prod'3 = loop 1
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc*x) xs

length'3 :: [a] -> Int
length'3 = loop 0
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc+1) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
  where loop !acc [] = acc
        loop !acc (x:xs) = loop (x+acc) xs