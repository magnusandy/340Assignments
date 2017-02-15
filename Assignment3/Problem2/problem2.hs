--Andrew Magnus
--amm215
--CMPT 340 Assignment 3

{-
Problem 2 [20 Points]. Define a function altMap, which takes two functions (instead of the one for map) to apply to a list.
altMap alternately applies its two argument functions to the successive elements of the list, the first function to the first element,
the second function to the second element, and so on.
For example, altMap (+10) (+100) [0, 1, 2, 3, 4] evaluates to [10, 101, 12, 103, 14]
-}
--TODO update problem 3 if this changes
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap fun1 fun2 list
  --if list is empty we cant go further
  | (length list) == 0  = []
  --if list has only one element left we apply fun1 to it and then end
  | (length list) == 1  = fun1 (head list) : []
  --otherwise we can apply both functions and recurse down
  | otherwise           = fun1 (head list) : fun2 ((head.tail) list) : altMap fun1 fun2 ((tail.tail) list)
