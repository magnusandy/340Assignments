--Problem 1 [10 Points]. Using the definitions of boolean constants and operators presented in class,
--show that the following evaluates to false. Show all your steps for a perfect mark.
--Always evaluate the "outer" applications first (i.e., use lazy evaluation), but continue to evaluate further until you obtain false.
 --and true (not true)

--Problem 2 [10 + 10 Points]. Define the higher-order library function curry that converts a function on pairs
--into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments
--into a function on pairs.

--TESTING FUNCTIONS
pairAdd :: (Int, Int) -> Int
pairAdd (x,y) = x+y

normAdd :: Int -> Int -> Int
normAdd x y = x+y

pairConcat :: (String, String) -> String
pairConcat (x, y) = x++y

normConcat :: String -> String -> String
normConcat x y = x++y

--Takes a function that operates on a pair (a, b) and returns an arbitrary value c. curry returns a curried version of the function where the values dont have to be passed as a pair
curry :: ((a,b) -> c) -> (a -> b -> c)
curry pairFunc = argsVersion pairFunc
  where
    argsVersion f a b = f (a, b)

--takes in a function that has two arguments a and b, and returns an arbitrary c. uncurry returns a new function that takes in a and b as a pair
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry argFunc = pairVersion argFunc
  where
    pairVersion f (a, b) = f a b

--Problem 3 [35 Points]. Declare a data type MyFloat -- for handling floating point numbers -- where each number is a
--(mantissa, exponent) pair of integers representing the floating point number mantissa x
--10exponent, so that a decimal point is assumed to exist just to the left of the leftmost digit of mantissa.
-- For example, (329, 23) would represent 0.329 x 10^23.  Both the mantissa and the exponent can be positive or negative.

--Carefully pick Haskell's built-in type classes which this type should be an instance of.
--You should define (and when possible overload) simple arithmetic and comparison operations
--on these floating numbers (at least: *, /, +, -, neg (negation), <=, >=, <, >, ==).
--Also define functions whole and fraction to extract the part of the represented number to the left of the decimal point
--(as an Int) and to the right of the decimal number (as a standard Haskell floating point number), respectively.
-- For example, for (329, 2), whole should return 32, and fraction should return 0.9.

data MyFloat = MyFloat (Integer, Integer)
--TODO ask if i can just do conversions baack and forth to normal doubles


whole :: MyFloat -> Integer
whole myF = floor (toDouble myF)

--TODO ask about this, fraction of 1.2 == 0.199999996 is that cool?
fraction :: MyFloat -> Double
fraction myF = ((toDouble myF) - (fromIntegral (whole myF)))

ezMult :: MyFloat -> MyFloat -> MyFloat
ezMult f1 f2 = fromDouble ((toDouble f1) * (toDouble f2))

ezAdd :: MyFloat -> MyFloat -> MyFloat
ezAdd f1 f2 = fromDouble ((toDouble f1) + (toDouble f2))

ezSub :: MyFloat -> MyFloat -> MyFloat
ezSub f1 f2 = fromDouble ((toDouble f1) - (toDouble f2))

ezDiv :: MyFloat -> MyFloat -> MyFloat
ezDiv f1 f2 = fromDouble ((toDouble f1) / (toDouble f2))

toDouble :: MyFloat -> Double
toDouble (MyFloat (m,ex)) =( ((fromIntegral m)/(10.0^^(digitsInMantissa (MyFloat (m, ex))))) * (10.0^^ex))

--converts a double into a MyFloat
fromDouble :: Double -> MyFloat
fromDouble 0 = MyFloat (0, 1)
fromDouble dubs = (normalize (withCount (abs dubs) (digitsInNumber (floor (abs dubs))))) * (MyFloat ((floor (signum (dubs))), 1))
  where
    withCount :: Double -> Integer -> MyFloat
    withCount dubNum leftDigits
      | leftDigits > 0 = MyFloat (floor (dubNum*(10.0^16)), leftDigits)
      | otherwise      = negExp dubNum 0 (dubNum*10.0)
    negExp :: Double -> Integer -> Double -> MyFloat
    negExp dubNum count biggerDub
      |  digitsInNumber(floor (biggerDub)) > 0 = MyFloat (floor (dubNum*(10.0^16)), count)
      | otherwise = negExp dubNum (count-1) ((biggerDub*10.0))
--returns a normalized version of a MyFloat i.e. MyFloat(300, 1) == 3 and MyFloat(30, 1) == 3 so we want them both normalized to MyFloat(3, 1)
--basically this function removes all trailing zeros from the first part of the pair
normalize :: MyFloat -> MyFloat
normalize (MyFloat (m, ex))
  | m == 0 = (MyFloat (m, 1))
  | (m `mod` 10) == 0 = normalize (MyFloat ((m `div` 10), ex))
  | otherwise         = (MyFloat (m,ex))

denormalize :: Integer -> MyFloat -> MyFloat
denormalize newDigits myF = denormHelp (newDigits - (digitsInMantissa myF)) myF
  where
    denormHelp::Integer -> MyFloat -> MyFloat
    denormHelp neededDigits (MyFloat (m, ex))
    --  | neededDigits < 0 = error "cannot denorm with negative count, please use normalize"
      | neededDigits > 0 = MyFloat ((m*(10^neededDigits)), ex)
      | otherwise = MyFloat (m, ex)

digitsInNumber ::Integer -> Integer
digitsInNumber myInt = digitsInMantissa (MyFloat (myInt, 0))
--counts the digits in the mantissa
digitsInMantissa :: MyFloat -> Integer
digitsInMantissa (MyFloat (m, ex)) =  count (abs m) 0
  where
    count 0 c = c
    count m c = count (m`div`10) (c+1)

-- displays MyFloats as a simple pair (menissa, exp)
instance Show MyFloat where
  --show :: MyFloat -> String
  show (MyFloat (x, y)) = show (x, y)

--normalizes the MyFloats then compares their menissas and exponents
instance Eq MyFloat where
--  (==) :: MyFloat -> MyFloat -> Bool
  (==) myF1 myF2 = isEqual (normalize myF1) (normalize myF2)
    where
      isEqual (MyFloat (m1, e1)) (MyFloat (m2, e2)) = ((m1 == m2) && (e1 == e2))
instance Ord MyFloat where
  --(<) :: MyFloat -> MyFloat -> Bool
  (<) myF1 myF2 = isLessThan (denormalize (returnLarger (digitsInMantissa myF1)(digitsInMantissa myF2)) myF1) (denormalize (returnLarger (digitsInMantissa myF1)(digitsInMantissa myF2)) myF2)
    where
      -- this is now acting on two MyFloats where the mantissas have the same number of digits
      isLessThan :: MyFloat -> MyFloat -> Bool
      isLessThan (MyFloat (m1, ex1)) (MyFloat (m2, ex2))
        -- if the exponents are not equal, the then the first float is smaller if the exponent is smaller
        | ex1 /= ex2 = (ex1 < ex2)
        --if the exponents are the same, we need to compare the mantissas. we need to denormalize the mantissas because we cant directly compare them if they are normalized i.e. 31 x 10^1 is > 3001 x10^1 but simply comparing 31<3001 will not find that so we need to denormalize 31, converting it to 3100
        | otherwise  = (m1 < m2)
--  (<=) :: MyFloat -> MyFloat -> Bool
  (<=) myF1 myF2 = (myF1 < myF2) || (myF1 == myF2)

--takes in a MyFloat and prints it
testFromInteger :: MyFloat -> [Char]
testFromInteger myF = show myF

--starts off with the case where one of the floats is 0, returns the other
--next it sets the mantissas to be of equal length, this makes the calculations go smoother i.e. (1,1) + (999, 3) becomes (100, 1) + (999,3)
instance Num MyFloat where
    fromInteger i = fromDouble (fromIntegral i)

    negate (MyFloat (m, ex)) = MyFloat (((-1)*m), ex )

    abs (MyFloat (m, ex))
      | m < 0    = negate (MyFloat (m, ex))
      | otherwise = (MyFloat (m, ex))

    signum (MyFloat (m, ex))
      | m > 0  = 1
      | m == 0 = 0
      | m < 0  = (-1)

    --(+) :: MyFloat -> MyFloat -> MyFloat
    --cases where one of the numbers is 0, return the other
    (+) (MyFloat (0, _)) myF2 = myF2
    (+) myF1 (MyFloat (0, _)) = myF1
    --regular case
    (+) (MyFloat (m1, ex1)) (MyFloat (m2, ex2)) = makeMantissaSameSize (denormalize (ex1) (MyFloat (m1, ex1))) (denormalize (ex2) (MyFloat (m2, ex2)))
      where
        --sets the matissas to the same length by adding zeros then calls myAdd
        makeMantissaSameSize :: MyFloat -> MyFloat -> MyFloat
        makeMantissaSameSize  (MyFloat (m1, ex1)) (MyFloat (m2, ex2))
          | (digitsInMantissa (MyFloat (m1, ex1))) /= (digitsInMantissa (MyFloat (m2, ex2))) = myAdd (denormalize (returnLarger (digitsInMantissa (MyFloat (m1, ex1))) (digitsInMantissa (MyFloat (m2, ex2)))) (MyFloat (m1, ex1))) (denormalize (returnLarger (digitsInMantissa (MyFloat (m1, ex1))) (digitsInMantissa (MyFloat (m2, ex2)))) (MyFloat (m2, ex2)))
          | otherwise  = myAdd (MyFloat (m1, ex1)) (MyFloat (m2, ex2))
        -- if the exponenents are the same we can add up the mantissas, if they are not the same we need to make them the same by adding the difference in exponents to the larger one i.e. (100, 1) + (999,3) becomes (100,3) + (99900, 3) then we recall myAdd
        myAdd :: MyFloat -> MyFloat -> MyFloat
        myAdd (MyFloat (m1, ex1)) (MyFloat (m2, ex2))
          | ex1 == ex2 = checkExp ((MyFloat ((m1+m2), ex1))) (returnLarger (digitsInMantissa (MyFloat (m1, ex1))) (digitsInMantissa (MyFloat (m2, ex2))))
          | ex1 < ex2 = myAdd (MyFloat (m1, ex2)) (denormalize (ex2+(ex2-ex1)+(digitsInMantissa(MyFloat (m2, ex2)))-ex2) (MyFloat (m2, ex2)))
          | ex1 > ex2 = myAdd (denormalize (ex1+(ex1-ex2)) (MyFloat (m1, ex1))) (MyFloat (m2, ex1))
        --once the mantissas have been added up we need to check if the exponent is correct, we can have the case where we cant simply use the larger of the two starting exponents, i.e. (100, 1) + (999,3) == 1 + 999 == 1000 == (1,4) so we need to make sure the exponent is correct
        -- finally this function returns the normalized answer of the addition
        checkExp :: MyFloat -> Integer -> MyFloat
        checkExp (MyFloat (m, ex)) lastLargest
          | (digitsInMantissa (MyFloat (m, ex))) > lastLargest = normalize (MyFloat (m, (ex+((digitsInMantissa (MyFloat (m,ex))-lastLargest)))))
          | otherwise  = normalize (MyFloat (m, ex))
    --adds the first to the second with the seconds mantissa changed to negative
    (-) myF1 (MyFloat (m, ex)) = myF1 + (MyFloat ((-1)*m, ex))
    -- multiplies two myFloats together, this can be done by first normalizing the pairs, then you can multiply the mantissas together and from there figure out the exponent which can be done by:
    (*) myF1 myF2 = normalize (lens (normalize myF1) (normalize myF2))
      where
        lens :: MyFloat -> MyFloat -> MyFloat
        lens myF1 myF2 = mult myF1 myF2 (digitsInMantissa myF1) (digitsInMantissa myF2)
        mult :: MyFloat -> MyFloat -> Integer -> Integer -> MyFloat
        mult (MyFloat (m1, ex1)) (MyFloat (m2, ex2)) len1 len2 = MyFloat (((fromIntegral m1)*(fromIntegral m2)), ((ex1-len1)+(ex2-len2)+(digitsInMantissa (MyFloat (((fromIntegral m1)*(fromIntegral m2)), 0)))))

instance Fractional MyFloat where
    fromRational x = fromDouble (fromRational x)

    (/) myF1 (MyFloat (0, _)) = error "cannot divide by zero"
    (/) myF1 myF2 = normalize (lens (normalize myF1) (normalize myF2))
      where
        lens :: MyFloat -> MyFloat -> MyFloat
        lens myF1 myF2 = divs myF1 myF2 (digitsInMantissa myF1) (digitsInMantissa myF2)
        divs:: MyFloat -> MyFloat -> Integer -> Integer -> MyFloat
        divs (MyFloat (m1, ex1)) (MyFloat (m2, ex2)) len1 len2 = MyFloat ( (newMantissa m1 m2) , ((ex1-len1)-(ex2-len2)+(digitsInMantissa (MyFloat ((floor ((fromIntegral m1)/(fromIntegral m2))), 0)))       ))
        --takes in both mantissas and divides them, it then multiplies by 1000000000 and chops off any remainder, this is essentially the same as taking it to 9 decimal places i.e. 4/3 = 1.333333333 it will take it to 1333333333.333 and chop off everything after the decimal giving 1333 we need a integer for the mantissa of the new thing
        newMantissa :: Integer -> Integer -> Integer
        newMantissa m1 m2 = floor (((fromIntegral m1)/(fromIntegral m2))*1000000000)

returnLarger :: Integer -> Integer -> Integer
returnLarger x y
  | x >= y = x
  | otherwise = y



--instance Num MyFloat where
  --(+) (MyFloat (x1,y1)) (MyFloat (x2, y2)) =


--Problem 4 [10 Points].  Write a polymorphic function, shuffle, which takes two lists l1 and l2 representing decks of cards,
--and returns the perfectly shuffled contents of the lists.
-- In other words, the returned list contains the first element of l1,
-- followed by the first element of l2, followed by the second elementof l1, and so on.
--If one of the lists ends before the other, the other list’s remaining elements are simply added to the end.

shuffle :: [a] -> [a] -> [a]
shuffle [] l2 = l2
shuffle l1 [] = l1
shuffle (l1Head:l1Tail) (l2Head:l2Tail) = l1Head : l2Head : (shuffle l1Tail l2Tail)

--Problem 5 [5 Points]. Write a polymorphic function, split, which takes as parameters a list and an Integer n,
--indicating the splitting point.  It splits the contents of the provided list into two lists,
--the first with the first n elements of the list (in order), and the second with the remaining elements (again, in order).
--These two lists are then returned as a pair.


split :: Integer -> [a] -> ([a], [a])
split n list
  | n < 0 = error "n must be positive"
  | n > toInteger(length list) = error "n shouldn't be longer than the list"
  | otherwise = ((firstPart n list), (secondPart n list))
  where
      -- returns the first n values in the list
      firstPart :: Integer -> [a] -> [a]
      firstPart 0 list = []
      firstPart n (h:t) = h : (firstPart (n-1) t)
      --returns the values left over after the first n are gone
      secondPart :: Integer -> [a] -> [a]
      secondPart 0 list = list
      secondPart n (h:t) = secondPart (n-1) t


--Problem 6 [10 Points]. Write a function, nshuffle, which takes two integers c and n.
--It first generates two lists, each of size c.
--One list contains c instances of the character ‘b’ (for black) and the other contains c instances of the character ‘r’ (for red).
--Then, it carries out n number of perfect shuffles, splitting each shuffled list into two equally sized lists before the next shuffle.
--For the shuffling and splitting, you must use functions shuffle and split written for the previous two problems.
--The function returns the final outcome of the n shuffles in the form of a single list.
--You may find it useful to define local functions for parts of this computation.

nshuffle :: Integer -> Integer -> [Char]
nshuffle c n = shuffler c n ((createList c 'b'), (createList c 'r'))
  where
    --takes in c for splitting, n for counting down and a pair of lists to be used by split
    shuffler :: Integer -> Integer -> ([Char], [Char]) -> [Char]
    shuffler c 0 (list1, list2) = list1++list2
    shuffler c 1 (list1, list2) = shuffle list1 list2
    shuffler c n (list1, list2) = shuffler c (n-1) (split c (shuffle list1 list2))
    createList :: Integer -> Char -> [Char]
    createList 0 char = []
    createList len char = char : createList (len-1) char

--Problem 7 [10 Points]. Write a function, consecutive, which takes a list of characters,
--and returns an integer indicating the largest number of consecutive identical characters in the list.

consecutive :: [Char] -> Integer
consecutive [] = 0
consecutive (firstChar:restOfList) = (consecCounter restOfList firstChar 1 1)
  where
    --keeps track of the last character, a current coun, and the best overall count
    consecCounter :: [Char]->Char->Integer->Integer->Integer
    consecCounter [] lastChar currentCount best = best
    consecCounter (h:t) lastChar currentCount best
      --if the current character is the same as the one before we can increase the counter and check to see if its the new best, otherwise we reset the
      --counter and go on the the next character in the list
      | h == lastChar = consecCounter t h (currentCount+1) (returnBetter (currentCount+1) best)
      | otherwise = consecCounter t h (1) (best)
    returnBetter :: Integer -> Integer -> Integer
    returnBetter x y
      | x >= y = x
      | otherwise = y
