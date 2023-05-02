{-# LANGUAGE DeriveFunctor #-}
actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!" 

echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

doEcho3 = do
  l1 <- getLine
  l2 <- getLine
  putStrLn $ l1 ++ l2 

dialog :: IO ()
dialog = putStr "What is your happy number? "
  >> getLine
  >>= \n -> let num = read n :: Int in
    if num == 7
    then putStrLn "Ah, lucky 7!"
    else if odd num
      then putStrLn "Odd number! That's most people choice"
    else putStrLn "Hm, interesting"

doDialog = do
  putStr "What is your happy number? "
  n <- getLine
  let num = read n :: Int in
    if num == 7
    then putStrLn "Ah, lucky 7!"
    else if odd num
      then putStrLn "Odd number! That's most people choice"
    else putStrLn "Hm, interesting"

data MyList a = EmptyList
  | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

data Either e a = Left e | Right a

instance Functor Either where
  fmap g (Left e) = fmap (Left e g)
  fmap g (Right a) = fmap (Right a g)