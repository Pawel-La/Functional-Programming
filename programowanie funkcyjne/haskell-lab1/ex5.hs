sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
            then 0
            else 1


absInt :: Int -> Int
absInt n = if n < 0
            then -n
            else n

min2Int :: (Int, Int) -> Int
min2Int (a,b) = if a < b
                then a
                else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a,b,c) = if min2Int(a,b) < c
                then min2Int(a,b)
                else c

toUpper :: Char -> Char
toUpper c = if fromEnum(c) >= 97 && fromEnum(c) <= 122
            then toEnum(fromEnum(c) - 32)
            else c

toLower :: Char -> Char
toLower c = if fromEnum(c) >= 65 && fromEnum(c) <= 90
            then toEnum(fromEnum(c) + 32)
            else c

isDigit :: Char -> Bool
isDigit n = if fromEnum(n) >= 48 && fromEnum(n) <= 57
            then True
            else False

charToNum :: Char -> Int
charToNum n = if fromEnum(n) >= 48 && fromEnum(n) <= 57
              then fromEnum(n) - 48
              else -1

romanDigit :: Char -> String
romanDigit n = if charToNum(n) == 1 
                then "I"
                else if charToNum(n) == 2 then "II"
                else if charToNum(n) == 3 then "III"
                else if charToNum(n) == 4 then "IV"
                else if charToNum(n) == 5 then "V"
                else if charToNum(n) == 6 then "VI"                          
                else if charToNum(n) == 7 then "VII"
                else if charToNum(n) == 8 then "VIII"
                else "IX"