-- ### 2016 Autumn ### --
-- Define a function tr which takes a list of lists, all of the same
-- length, and returns their "transpose", meaning a list of lists of
-- the first elements, the second elements, etc. (All lists in test
-- cases can be assumed to be non-empty.) Be sure to include a
-- type signature.
-- Examples:
-- tr [[1,2,3],[4,5,6]]
-- => [[1,4],[2,5],[3,6]]
-- tr ["foxes","socks","rocks"]
-- => ["fsr","ooo","xcc","ekk","sss"]

tr :: [[a]] -> [[a]]
tr li
  | (null (head li)) = [] -- If the head of the list is empty, so is everything else. Return an empty list
  | otherwise = [map head li] ++ tr (map tail li) -- Otherwise, get the head of each inner list, put it in a list, and add those lists to a list. Then recurse with the tail of each inner list

-- ### 2016 January ### --
-- Define a Haskell function revCount which takes two lists, the
-- second of which is a list of non-negative Ints the same length as
-- the first list, and returns a list of elements from the first list, in
-- reverse order, each repeated a number of times as specified by
-- the corresponding element of the second list.
-- Be sure to include a type declaration for revCount.
-- Examples:
-- revCount ['a','b','c'] [1,2,3] => ['c','c','c','b','b','a']
-- revCount ['d','c','b','a'] [3,0,0,1] => ['a','d','d','d']

revCount :: [a] -> [Int] -> [a]
revCount li ln
  | (null li) = [] -- If the list is null, return an empty list
  | otherwise = replicate (last ln) (last li) ++ revCount (init li) (init ln) -- Otherwise, create a list of the last element of the target list repeated by the last element of the integer list, and recurse with everything but the last element of each list
