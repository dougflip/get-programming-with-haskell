module Chapter08 where

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myTake :: (Integral n) => n -> [a] -> [a]
myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n - 1) xs

finiteCycle :: [a] -> [a]
finiteCycle (first:rest) = first : rest ++ [first]

myCycle :: [a] -> [a]
myCycle (x:xs) = x : myCycle (xs ++ [x])

ackermann :: (Integral n) => n -> n -> n
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz :: (Integral n) => n -> n
collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

{-|
  Not sure if this is exactly correct.
  It produces a fib number, but not sure it matches the count.
  It may be off by 1 depending on what you expect count to match up with
-}
fastFib :: (Integral n) => n -> n -> n -> n
fastFib x y 0     = x + y
fastFib x y count = fastFib y (x + y) (count - 1)
