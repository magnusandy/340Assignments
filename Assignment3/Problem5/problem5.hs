--Andrew Magnus
--amm215
--CMPT 340 Assignment 3

{-
Problem 5 [20 Points].
A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
For example, 6, 28, 496, and so on.  Define a list comprehension to generate an infinite list of perfect numbers.
Do not make external function calls.  Try to be efficient in the search for factors.
[
Hint: One way to do this is by
(1) placing a comprehension for finding factors in a qualifier of the the top-level comprehension,
(2) adding the factors using an appropriate higher-order function, and
(3) checking to see whether the sum equals the number.
]
-}
--calculates perfect ints in a large messy comprehension
--Steps:
--  The inner comprehension finds factors of perfectVal but checking if anything less than (value/2 because all factors will be less than value/2) is divisible into the perfectVal evenly and returning a list of all these factorSum
--  Next this list of factors is summed up using foldr
--  Next this summation of factors is compared with the original value, if the summation and value are the same the number is perfect and is kept
-- Finally return this list of perfect values
perfectInts :: [Integer]
perfectInts = [perfectVal | perfectVal <- [1 .. ], (foldr (+) (0) ([ factor | factor <- [1..(perfectVal`div`2)], ((factor < perfectVal) && (perfectVal `mod` factor) == 0)] )) == perfectVal]

--ALTERNATE VERSION WHICH IS EASIER TO UNDERSTAND
--calculates list of perfect ints using  helper functions
perfectIntsReadable :: [Integer]
perfectIntsReadable = [perfectVal | perfectVal <- [1 .. ], ((factorSum perfectVal) == perfectVal)]
  where
    --returns the sum of all factors of the given val
    factorSum :: Integer -> Integer
    factorSum val = foldr (+) 0 (factorComprehension val)
      where
        --creates a list of all factors of the given integer val
        factorComprehension :: Integer -> [Integer]
        factorComprehension val = [ x | x <- [1..(val`div`2)], ((x < val) && ((val `mod` x) == 0))]
        --returns the sum of all the factors of val
