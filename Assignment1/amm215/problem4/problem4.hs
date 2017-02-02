--Problem 4
--PART A) Average 3 function to find the average of three integers
averageThree :: Integer -> Integer -> Integer -> Double
averageThree first second third = fromInteger (first + second + third)/3.0 -- need the fromInteger to convert to Double or the number returned will be int

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage first second third =
  ((first `isAbove` average) + (second `isAbove` average) + (third `isAbove` average))
  where
    average :: Double
    average = averageThree first second third
    isAbove :: Integer -> Double -> Integer
    isAbove val1 val2
      | fromInteger val1 > val2 = 1
      | otherwise     = 0
--PART B) average three using a triplet
averageThreeInOne :: (Integer, Integer, Integer) -> Double
averageThreeInOne (first, second, third) = averageThree first second third

--PART C)
-- basic logic of  the sort, if howManyAboveAverage val1 val2 val2 = 1 then val1 is > val2, if 2 then val 2 is > val 1 if 0 then val1 == val2
--TODO WHAT DOES IT MEAN SCOPED INSIDE THE MAIN FUNCTION?
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (first, second, third)
  | (howManyAboveAverageInner first second second  == 1) = orderTriple(second, first, third) --IF 1 that means first is above second, any other case, 2 or 0, mean no rearranging is necessary
  | (howManyAboveAverageInner second third third == 1) = orderTriple(first, third, second)
  | otherwise = (first, second, third)
  where
    --same code as part A function
    howManyAboveAverageInner :: Integer -> Integer -> Integer -> Integer
    howManyAboveAverageInner first second third =
      ((first `isAbove` average) + (second `isAbove` average) + (third `isAbove` average))
      where
        average :: Double
        average = fromInteger (first + second + third)/3.0 -- same code as averageThree
        isAbove :: Integer -> Double -> Integer
        isAbove val1 val2
          | fromInteger val1 > val2 = 1
          | otherwise     = 0
