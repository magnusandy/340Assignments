--Andrew Magnus
--amm215
--CMPT 340 Assignment 3

{-
Problem 4 [20 Points]. Show how the single comprehension [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
with two generators can be alternatively expressed using two comprehensions with single generators.
[Hint: make use of the library function concat :: [[a]] -> [a], and nest one comprehension within the other.]
-}

--given example from the assignment
--takes an item from first and pairs it with every item y from the second list then moves on to the next x
singleComp2Gens :: [(Int,Int)]
singleComp2Gens = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

--same list as the singleComp2Gens but using nested list comprehensions
--the outer comprehension creates list of lists, for each inner list a single element first is paired with every  element of second
-- concat flattens the list of lists into a single list
twoCompSingleGens :: [(Int,Int)]
twoCompSingleGens = concat [ [ (first ,second) | second <- [4,5,6] ] | first <- [1,2,3] ]
