-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Exercise 2
-- You could write this using toDigitsRev and sum, but seems to be a waste of time...
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  case reverse xs of
    []       -> []
    [x]      -> [x]
    (x:y:zs) -> doubleEveryOther (reverse zs) ++ [2 * y, x]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)
-- equivalent to sumDigits xs = sum $ map (sum . toDigits) xs

-- Exercise 4
-- Will add a negative check, but I guess we don't care about num of digits (trivial)
validate :: Integer -> Bool
validate n = n > 0 && sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0
-- To try: write with (.)
