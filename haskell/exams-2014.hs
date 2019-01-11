-- ### 2014 Autumn ### --
-- Define a function
-- sort :: (a -> a -> Bool) -> [a] -> [a]
-- which sorts a list according to the given predicate.

sort :: (a -> a -> Bool) -> [a] -> [a]
sort _ [] = [] -- If the list is empty, return an empty list
sort p (l : li) = -- Bind l to be an element of li
  let smaller = sort p (filter (p l) li) -- Find the smallest element (relative to the predicate) by recursing with a list of elements filtered by the predicate
      bigger = sort p (filter (not . p l) li) -- Find the biggest element (relative to the predicate) by recursing with a list of elements filtered by the negated predicate
  in smaller ++ [l] ++ bigger -- Order the list from smallest to biggest

-- ### 2014 January ### --
-- Define a higher-order function mapEveryOther with the type
-- mapEveryOther :: (a->a) -> [a] -> [a]
-- which applies the given function to *every other* element of
-- the given list, starting with the first element.
-- Examples:
-- mapEveryOther (+1000) [0,1,2,3,4,5,6] =
-- 1000,1,1002,3,1004,5,1006]
-- It should work on lists of either even or odd length.

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther func li
  | (null li) = [] -- If the list is null, return an empty list
  | (null (tail li)) = func (head li) : [] -- If the tail is null, apply the function to the head of the list and add it to an empty list
  | otherwise = func (head li) : li !! 1 : mapEveryOther func (drop 2 li) -- If the list contains more than one element, apply the function to the head, get the second element from the list, and add these to a new list and recurse after dropping the first two elements from the list
