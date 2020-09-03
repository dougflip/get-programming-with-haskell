module Chapter06 where

repeat' :: a -> [a]
repeat' a = cycle [a]

subseq :: Int -> Int -> [a] -> [a]
subseq start end = take (end - start) . drop start

inFirstHalf :: (Eq a) => a -> [a] -> Bool
inFirstHalf el xs = el `elem` firstHalf
  where
    half = (length xs) `div` 2
    firstHalf = take half xs
