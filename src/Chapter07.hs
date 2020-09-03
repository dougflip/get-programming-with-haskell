module Chapter07 where

gcd' :: (Integral a) => a -> a -> a
gcd' x y =
  if remainder == 0
    then y
    else gcd' y remainder
  where
    remainder = x `mod` y

tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs

myGcd :: (Integral a) => a -> a -> a
myGcd x 0 = x
myGcd x y = myGcd y (x `mod` y)
