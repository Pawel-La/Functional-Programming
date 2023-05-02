fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _ = False

firstDivsSecond :: Integral a => [a] -> Bool
firstDivsSecond (x:y:_) | (y `mod` x) == 0 = True
firstDivsSecond _ = False

firstDivsThird :: Integral a => [a] -> Bool
firstDivsThird (x:y:z:_) | (z `mod` x) == 0 = True
firstDivsThird _ = False