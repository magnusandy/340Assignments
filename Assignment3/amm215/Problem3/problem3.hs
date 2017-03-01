--Andrew Magnus
--amm215
--CMPT 340 Assignment 3

{-
Problem 3 [10 Points]. Using altMap, define the function luhn :: [Int] -> Bool for the Luhn algorithm,
which was assigned for Assignment 1.

LUHN from Assignment 1
Problem 3. (5 + 10 Points) The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit,
and proceeds as follows:
- Consider each digit as a separate number
- Moving right-to-left and beginning at the second last digit, double every other number
- Subtract 9 from each number that is now greater than 9
- Add all the resulting numbers together
- If the total is divisible by 10, the card number is valid
a) Define a function luhnDouble that doubles a digit and subtracts 9 if the result is greater than 9.  For example:
> luhnDouble 3
6
> luhnDouble 6
3
b) Using luhnDouble, define a function luhn which decides if a four-digit bank card number is valid.  For example:
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False
-}

altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap fun1 fun2 list
  --if list is empty we cant go further
  | (length list) == 0  = []
  --if list has only one element left we apply fun1 to it and then end
  | (length list) == 1  = fun1 (head list) : []
  --otherwise we can apply both functions and recurse down
  | otherwise           = fun1 (head list) : fun2 ((head.tail) list) : altMap fun1 fun2 ((tail.tail) list)


--TODO ask about the case where list is [] should it be true or false
luhn :: [Integer] -> Bool
luhn list
--because luhn algorithim looks from right to left, if the list is of even length, the secondlast number will
--be an odd index so we want to apply the lughdouble to all odd index items and just return the item for the rest
  | ((even.length) list) == True = ((foldr (+) 0 (altMap (luhnDouble) (\x -> x) list)) `mod` 10) == 0
  --otherwise flip it around
  | otherwise                    = ((foldr (+) 0 (altMap (\x -> x) (luhnDouble) list)) `mod` 10) == 0
    where
      luhnDouble :: Integer -> Integer
      luhnDouble val
        | ((val > 9) || (val < 0)) = error "value should be single digit 0 - 9"
        | val*2 > 9 = (2*val)-9
        | otherwise = 2*val
