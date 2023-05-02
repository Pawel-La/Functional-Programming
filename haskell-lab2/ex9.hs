qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where
    leftPart xs = filter (<= x) xs
    rightPart xs = filter (>x) xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = 
  if x <= (head xs) then isSorted xs
  else False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:xs) [] = []
zip' [] (x:xs) = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys
