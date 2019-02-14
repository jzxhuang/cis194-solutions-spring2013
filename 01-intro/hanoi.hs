-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n pegSource pegTarget pegTemp
    -- Check for invalid input
    | n <= 0 = []
    -- Base case, n = 1: Move from source peg to target peg
    | n == 1 = [(pegSource, pegTarget)]
    -- Otherwise, recursion! Move n-1 discs from source to temp, move bottom disc from source to target, move n-1 discs from temp to target
    | otherwise = hanoi (n-1) pegSource pegTemp pegTarget ++ hanoi 1 pegSource pegTarget pegTemp ++ hanoi (n-1) pegTemp pegTarget pegSource


-- Exercise 6: To-do
