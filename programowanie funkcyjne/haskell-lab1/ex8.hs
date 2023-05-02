not' :: Bool -> Bool
not' b = case b of 
          True -> False
          False -> True

absInt n =
  case (n >= 0) of
    True -> n
    _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s = 
  case s of
    "Love" -> True
    _      -> False

or' :: (Bool, Bool) -> Bool
or' (x,y) =
  case (x == False && y == False) of
    True -> False
    _    -> True

and' :: (Bool, Bool) -> Bool
and' (x,y) =
  case (x == True && y == True) of
    True -> True
    _    -> False