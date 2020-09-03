module Chapter13 where

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is the opposite of False"
  describe False = "This is the opposite of True"
