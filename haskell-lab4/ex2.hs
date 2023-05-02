data MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2
instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i) = MkMyInt (negate i)
  abs (MkMyInt i) = MkMyInt (abs i)
  signum (MkMyInt i) = MkMyInt (signum i)
  fromInteger int = MkMyInt (fromIntegral int)
instance Show MyInt where
  show (MkMyInt i) = "MkMyInt: "++ show i 

instance Eq T1 where
(==) (B (a1, b1)) (B(a2, b2)) = a1 == a2 && b1 == b2
(==) A A = True
(==) _ _ = False