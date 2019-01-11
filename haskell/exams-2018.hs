-- ### 2018 Autumn ### --
-- Define a Haskell function mapSkip which takes a function and a
-- list and returns the result of applying the given function to
-- every other element of the given list, starting with the first
-- element. Be sure to include a type signature.
-- Example:
-- mapSkip (+1000) [1..6]
-- => [1001,2,1003,4,1005,6]

mapSkip :: (a -> a) -> [a] -> [a]
mapSkip func li
  | null li = [] -- If the list is null, return an empty list
  | (length li) == 1 = func (head li) : [] -- If the list is of size one, apply the function to the head of the list and add it to an empty list
  | otherwise = func (head li) : li !! 1 : mapSkip func (drop 2 li) -- If the list contains more than one element, apply the function to the head, get the second element from the list, and add these to a new list and recurse after dropping the first two elements from the list

-- ### 2018 January ### --
-- Define a Haskell function tear, including its type, which takes a
-- predicate and a list and returns a list of two lists, the first
-- those elements of the input list which pass the predicate, the
-- second those that don't, in order.
-- Examples:
-- tear (>5) [1,10,2,12,3,13]
-- => [[10,12,13],[1,2,3]]

tear :: (a -> Bool) -> [a] -> [[a]]
tear pre li = (filter pre li) : (filter (not . pre) li) : [] -- Make two lists, one of the elements filtered by what passes the predicate, the other filtered by what elements dont pass. Then add both lists to an empty list
