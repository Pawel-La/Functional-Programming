absInt :: Int -> Int
absInt n | n >= 0 = n | otherwise = -n

sgn :: Int -> Int
sgn n | n > 0 = 1 | n < 0 = -1 | otherwise = 0

min3Int :: (Int,Int,Int) -> Int
min3Int (x,y,z) | (x < y && x < z) = x | y < z = y | otherwise = z

toUpper :: Char -> Char
toUpper n | fromEnum(n) > 90 = toEnum(fromEnum(n) - 32)
          | otherwise = n

toLower :: Char -> Char
toLower n | fromEnum(n) <= 90 = toEnum(fromEnum(n) + 32)
          | otherwise = n
