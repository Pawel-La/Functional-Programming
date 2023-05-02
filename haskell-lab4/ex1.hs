data BinTree a = EmptyBT |
                    NodeBT a (BinTree a) (BinTree a) deriving Show

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

depthOfBinTree :: BinTree a -> Int
depthOfBinTree EmptyBT = 0
depthOfBinTree (NodeBT n lt rt) = 1 + max (depthOfBinTree lt) (depthOfBinTree rt)

flattenBT1 :: BinTree a -> [a]
flattenBT1 EmptyBT = []
flattenBT1 (NodeBT n lt rt) = flattenBT1 lt ++ [n] ++ flattenBT1 rt

flattenBT2 :: BinTree a -> [a]
flattenBT2 EmptyBT = []
flattenBT2 (NodeBT n lt rt) = [n] ++ flattenBT2 lt ++ flattenBT2 rt

flattenBT3 :: BinTree a -> [a]
flattenBT3 EmptyBT = []
flattenBT3 (NodeBT n lt rt) = flattenBT3 lt ++ flattenBT3 rt ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT n lt rt) 
      | x == n = NodeBT n lt rt
      | x < n = NodeBT n (insert x lt) rt
      | x > n = NodeBT n lt (insert x rt)


data Expr a = Lit a |
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2 

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"