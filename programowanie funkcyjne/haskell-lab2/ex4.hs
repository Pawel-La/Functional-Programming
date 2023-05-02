isPalindrome :: [Char] -> Bool
isPalindrome s | s == reverse s = True | otherwise = False


getElemAtIdx t idx = head (drop idx t)

capitalize :: [Char] -> [Char]
capitalize w = 