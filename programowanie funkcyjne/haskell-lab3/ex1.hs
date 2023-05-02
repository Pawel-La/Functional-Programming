import Data.List

-- sumWith :: Num a => (a -> a) -> [a] -> a
-- sumWith _ [] = 0
-- sumWith f (x:xs) = f x + (sumWith f xs)

-- prodWith :: Num a => (a -> a) -> [a] -> a
-- prodWith _ [] = 1
-- prodWith f (x:xs) = f x * (prodWith f xs)

sqr x = x^2
funcFactory n = case n of
  1 -> id
  2 -> sqr
  3 -> (^3)
  4 -> \x -> x^4
  5 -> intFunc
  _ -> const n
  where
    intFunc x = x^5


dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> ( f(x + h) - f(x - h) ) / (2*h)


funcList :: [Double -> Double]
funcList = [\x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x]

-- evalFuncListAt :: a -> [a -> b] -> [b]
-- evalFuncListAt x [] = []
-- evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

sqrElems xs = [x^2 | x <- xs]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x xs = map ($ x) xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith = go 0
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x + acc) g xs

prodWith :: Num a => (a -> a) -> [a] -> a 
prodWith = go 1
  where
    go acc g [] = acc
    go acc g (x:xs) = go (acc * g x) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll f z [] = z
foldll f z (x:xs) = foldll f (f z x) xs

sumWith' g = foldll (\x y -> x + g y) 0
prodWith' g = foldll (\x y -> x * g y) 1 
