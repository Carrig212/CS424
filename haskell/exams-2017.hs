-- ### 2017 Autumn ### --
-- Define a Haskell function foo, including a type signature, that takes two lists and yields
-- a list combining all the elements in the two input lists, taking 1 from the first list, 2 from
-- the second list, 3 from the first list, 4 from the second list, etc,until both are exhausted.
-- Examples:
-- foo [1,2,3,4,5,6,7,8] [11,12,13,14,15,16,17,18]
-- => [1,11,12,2,3,4,13,14,15,16,5,6,7,8,17,18]

foo :: [a] -> [a] -> [a]
foo li1 li2 = oof li1 li2 1 -- Pass the lists to an inner function with a starting x value
  where
    oof :: [a] -> [a] -> Int -> [a]
    oof l1 l2 x
      | (null l1) && (null li2) = [] -- If both lists are null, return an empty list
      | (null l1) = l2 -- If the first is null, return the second
      | (null l2) = l1 -- If the second is null, return the first
      | (odd x) = take x l1 ++ oof (drop x l1) l2 (succ x) -- If the x value is odd, take x elements from list one and add them to a list and recurse, dropping x elements from the first list and incrementing x
      | (even x) = take x l2 ++ oof l1 (drop x l2) (succ x) -- If the x value is even, take x elements from list two and add them to a list and recurse, dropping x elements from the second list and incrementing x

-- ### 2017 January ### --
-- Define a Haskell function weaveHunks which takes an int and
-- two lists and weaves them together in hunks of the given size.
-- Be sure to declare its type signature.
-- Examples:
-- weaveHunks 3 "abcdefghijklmno" "ABCDEFGHIJKLMNO"
-- => "abcABCdefDEFghiGHIjklJKLmnoMNO"
-- weaveHunks 2 [1..10] [11..20]
-- => [1,2,11,12,3,4,13,14,5,6,15,16,7,8,17,18,9,10,19,20]

weaveHunks :: Int -> [a] -> [a] -> [a]
weaveHunks hunk li1 li2
  | (null li1) && (null li2) = [] -- If both lists are null, return an empty list
  | (null li1) = li2 -- If the first is null, return the second
  | (null li2) = li1 -- If the second is null, return the first
  | otherwise = take hunk li1 ++ weaveHunks hunk li2 (drop hunk li1) -- Otherwise, take a hunk from the first list, add it to a list, and recurse with the lists swapped and the hunk of elements dropped from the first list
