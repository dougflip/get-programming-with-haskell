module Chapter10 where

cup :: (Num a) => a -> (a -> b) -> b
cup flOz = \message -> message flOz

getOz :: ((a -> a) -> b) -> b
getOz aCup = aCup (\flOz -> flOz)

-- ghci told me this, this was a tough one to work out on my own, but I should
sip :: (Ord a1, Num a1) => a1 -> ((a2 -> a2) -> a1) -> (a1 -> b) -> b
sip amount aCup =
  if remainingCoffee >= 0
    then cup remainingCoffee
    else cup 0
  where
    oz = getOz aCup
    remainingCoffee = oz - amount

robot (name, attack, hp) = \message -> message (name, attack, hp)

-- helpers
name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

-- accessors
getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

printRobot aRobot =
  aRobot (\(n, a, h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0
