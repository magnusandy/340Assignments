--Problem 3
--PART A) luhnDouble
luhnDouble :: Integer -> Integer
luhnDouble val
  | ((val > 9) || (val < 0)) = error "value should be single digit 0 - 9"
  | val*2 > 9 = (2*val)-9
  | otherwise = 2*val

--PART B) Luhn algorithm
luhn :: Integer -> Integer -> Integer -> Integer -> Bool
luhn first second third forth = total `mod` 10 == 0
  where
    total :: Integer
    total = (luhnDouble first) + second + (luhnDouble third) + forth
