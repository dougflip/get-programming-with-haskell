module Chapter09 where

import           Data.Char (toLower)

addAnA :: [String] -> [String]
addAnA []     = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = (f x) : myMap f xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' el xs = length (filter (\x -> el == x) xs) >= 1

isPalindrome :: String -> Bool
isPalindrome s = noSpaces == reverse noSpaces
  where
    lower = map (toLower) s
    noSpaces = filter (\x -> x /= ' ') lower

harmonic :: (Enum b, Fractional b) => b -> b
harmonic n = foldr (+) 0 series
  where
    seq = [1 .. n]
    series = map (1 /) seq
