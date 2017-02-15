--Andrew Magnus
--amm215
--CMPT 340 Assignment 3

{-
Problem 1 [15 + 15 Points]. A higher-order function unfold can be defined as follows to encapsulate a pattern of recursion
for producing a list:

unfold p h t x   | p x            = [ ]
                 | otherwise      = h x : unfold p h t (t x)

That is, the function unfold p h t produces the empty list if the predicate p is true of the argument,
and otherwise produces a non-empty list by applying the function h to give the head,
and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list.
For example, a function int2bin (to convert integers to binary numbers) can be written as follows:

int2bin  =  unfold (==0) (`mod`2) (`div`2)

[Note: putting function names mod and div inside back quotes allows them to be used infix]

Define the following functions using unfold:


b) iterate f, where iterate f x produces a list by applying the function f to x an increasing number of times, as follows:

                  iterate f x = [x, f x, f (f x), f (f (f x)), ... ]

-}

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold predicate headFunction tailFunction x
                | ((predicate x) == True)          = [ ]
                | otherwise                        = (headFunction x) : unfold predicate headFunction tailFunction (tailFunction x)
--example of unfold
int2bin :: Integer -> [Integer]
int2bin  =  unfold (==0) (`mod`2) (`div`2)

--a) map f
--takes in funcion a that acts on a's and returns b's, and a list of a's, returns a list of b's
map ::(Eq a) => (a -> b) -> [a] -> [b]
--if the list is empty we are done, otherwise, apply the function to the head and keep going on the rest of the list
map function = unfold (==[]) (function.head) (tail)

--b) iterate f, where iterate f x produces a list by applying the function f to x an increasing number of times, as follows:
--takes in a function that is, that function is repetidly applied to a to create a list of a's
-- iterate foo x = [x, foo x, foo (foo x), foo (foo (foo x)), ... ]
iterate :: (a->a) -> a -> [a]
iterate foo x =  x : unfold ((\x -> False)) (foo) (foo) x
