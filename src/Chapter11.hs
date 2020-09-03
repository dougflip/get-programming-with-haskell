module Chapter11 where

halve :: (Integral i) => i -> i
-- halve = (flip div) 2
halve val = val `div` 2

printDouble :: (Num i, Show i) => i -> String
printDouble i = show $ 2 * i
