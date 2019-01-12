-- ### 2018 Assignment ### --
-- Consider lists of digits (base b) representing natural numbers.
-- The list starts with the least significant digit, and proceeds up.
-- Unlike standard natural numbers, these NI's can be nonstandard by
-- having an infinite list of (nonzero) digits.
-- Examples (in base 10):
-- one = [1], or equivalently [1,0] or even [1,0,0,0,...]
-- twelve = [2,1] (with an arbitrary number of trailing zeros.)
-- Define
-- Examples:
-- plus 10 [7,2,1] [9,1,1] = [6,4,2]
-- plus 10 [1] (repeat 9) = repeat 0
-- times 10 [2] (repeat 9) = [8] ++ repeat 9

-- ### Question 1 ### --
-- plus :: Int -> [Int] -> [Int] -> [Int]
-- which takes a base (>=2) and two NIs, and returns their sum

plus :: Int -> [Int] -> [Int] -> [Int]
plus base n1 n2 = sulp base n1 n2 0 -- Pass the parameters to an inner function with an initial carry of 0
  where
    sulp :: Int -> [Int] -> [Int] -> Int -> [Int]
    sulp base n1 n2 carry -- FYI, "sulp" is just "plus", but backwards
      | (null n1) && (null n2) && (carry == 0) = [] -- If both lists are empty and the carry is 0, the addition is done, return an empty list
      | (null n1) && (null n2) = [carry] -- If the carry isn't 0, return it in a list
      | (null n1) = (mod ((head n2) + carry) base) : (sulp base n1 (tail n2) (quot ((head n2) + carry) base)) -- If n1 is null, add the carry to the least significant digit of n2 and keep it in base, then recurse with the rest of the digits of n2 and the new carry
      | (null n2) = (mod ((head n1) + carry) base) : (sulp base (tail n1) n2 (quot ((head n1) + carry) base)) -- If n2 is null, add the carry to the least significant digit of n1 and keep it in base, then recurse with the rest of the digits of n1 and the new carry
      | otherwise = (mod ((head n1) + (head n2) + carry) base) : (sulp base (tail n1) (tail n2) (quot ((head n1) + (head n2) + carry) base)) -- Otherwise, add the least significant digit of each n to the carry and keep it in base, then recurse with the least significant digits of each and the new carry

-- ### Question 2 ### --
-- times :: Int -> [Int] -> [Int] -> [Int]
-- which is similar, but returns their product

times :: Int -> [Int] -> [Int] -> [Int]
times base n1 n2 = semit base n1 n2 0 -- Pass the parameters to an inner function with a starting x value of 0
  where
    semit :: Int -> [Int] -> [Int] -> Int -> [Int]
    semit base n1 n2 x -- FYI, "semit" is just "times" backwards
      | (null (mult base n1 n2 x)) = [] -- If multiplication returns null, return an empty list
      | otherwise = (head (mult base n1 n2 x)) : (semit base n1 n2 (succ x)) -- Otherwise, create a new list with the result of multiplying the current element, and iterate recursively

mult :: Int -> [Int] -> [Int] -> Int -> [Int]
mult base n1 n2 x = (drop x (nMult base (take (succ x) n1) (take (succ x) n2))) -- Remove x elements from the result of multplying the next x values of each list

nMult :: Int -> [Int] -> [Int] -> [Int]
nMult base n1 n2 -- "NI multiplication"
  | (null n2) = [] -- If the number being multiplied is empty, the multiplication is finished, return an empty list
  | otherwise = (plus base (sMult base n1 (head n2) 0) (nMult base (0 : n1) (tail n2))) -- Otherwise, get the sum of the results of the scalar multiplication, recursively iterating down the number being multiplied and padding the multiplier with a zero

sMult :: Int -> [Int] -> Int -> Int -> [Int]
sMult base n1 n2 carry -- "Scalar multiplication"
  | (null n1) && (carry == 0) = [] -- If the multiplier is empty, and the carry is zero, the multiplication is finished, return an empty list
  | (null n1) = [carry] -- If the carry isn't zero, return it inside a list
  | otherwise = (mod ((head n1) * n2 + carry) base) : (sMult base (tail n1) n2 (quot ((head n1) * n2 + carry) base)) -- Otherwise, start a new list with the scalar multiplication result of the first element of the multiplier and the current element of the number being multiplied within the base, and recurse with the rest of the muliplier and the new carry
