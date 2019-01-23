-- ### 2019 January ### --
-- Define a scheme function scatterGather which takes three
-- arguments, replace, a list indicies of indicies, and a list vals of values,
-- and returns a list the same length as indicies but with each
-- value K replaced by the K-th element of vals, or if that is out of
-- range, by replace. Be sure to give a type signature
-- Example:
-- scatterGather '_' [0,1,4,1,1,7,2] "abcde"
-- => "abebb_c"
-- scatterGather 0 [0,1,4,1,1,7,2] [100,101,102]
-- => [100,101,0,101,101,0,102]

scatterGather :: a -> [Int] -> [a] -> [a]
scatterGather x indicies vals
  | (null indicies) || (null vals) = [] -- If either the indicies, or the values are null, return an empty list
  | ((head indicies) > (length vals)) || ((head indicies) < 0) = [x] ++ (scatterGather x (tail indicies) vals) -- If the index at the head of indicies is out of the range of vals, start a new list with the x value, and recurse with the tail of indicies
  | otherwise = [vals !! (head indicies)] ++ (scatterGather x (tail indicies) vals) -- otherwise, start a new list with the vals vlaue at the index specified by the head of indicies, and recurse with the tail of indicies
