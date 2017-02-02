--Problem 2
--PART A) conditional expression
fastExp1 :: Double -> Integer -> Double
fastExp1 value expo =
  if (expo <= 0)
    then error "exponent must be above 0"
  else if (expo == 1)--value^1 BASE CASE
    then value
    else
      if (even expo) --
        then
          (fastExp1 value (expo `div` 2))^2 --apparently this will work in haskell? ^ operator, nice
        else
          (value * (fastExp1 value (expo-1)))

--PART B) Guarded Expression
fastExp2 :: Double -> Integer -> Double
fastExp2 value expo
  | (expo <= 0) = error "exponent must be above 0"
  | (expo == 1) = value --BASE CASE
  | (even expo) = (fastExp2 value (expo `div` 2))^^2
  | otherwise   = (value * (fastExp2 value (expo-1)))

--PART C) Pattern Matching
fastExp3 :: Double -> Integer -> Double
fastExp3 value 1 = value --matches if exponent is 1, BASE CASE
fastExp3 value expo = isExpoEvenOrNegative (even expo) (expo <= 0)
  where
    isExpoEvenOrNegative _ True = error "exponent must be above 0" -- if negative throw error and even or odd doesnt matter
    isExpoEvenOrNegative True _ = (fastExp3 value (expo `div` 2))^2
    isExpoEvenOrNegative False _ = (value * (fastExp3 value (expo-1)))
