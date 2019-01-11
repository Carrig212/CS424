-- ### 2015 Autumn ### --
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

-- ### 2015 January ### --
-- Define a Haskell function afterFilter which takes a predicate p
-- and a list xs and returns a list of those elements of xs which
-- immediately follow an element which passes the predicate p.
-- Be sure to include a type signature for afterFilter.
-- Examples:
-- afterFilter (<0) [-4,7,-4,-8,3,-3,-6,0,-9,-1]
-- => [7,-8,3,-6,0,-1]
-- afterFilter (=='f') "fifferfefferfather"
-- => "ifeefea"

afterFilter :: (a -> Bool) -> [a] -> [a]
afterFilter p li
  | (null li) || (null (tail li)) = [] -- If the list is null or we're at the end of the list, return an empty list
  | (p (head li)) = [(head (tail li))] ++ afterFilter p (tail li) -- If the first element of the list passes the predicate, create a list with the element after that one, and recurse with the rest of the list
  | otherwise = afterFilter p (tail li) -- Otherwise, just recurse with the rest of the list
