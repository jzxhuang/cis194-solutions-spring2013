module Golf where

-- Exercise 1: Hopscotch
-- Used two different implementations, was not familiar inittialy with list comprehension synta


-- Solution 1a) Using zip and filter
-- Zip with [1.. length list], to get the indice of each element
skips :: [a] -> [[a]]
skips list =
    map (\(i, _) -> getNthElements i p ) p
        where p = zip [1..length list] list


-- Get the nth elements from a zipped list
-- Filter out the values we want, then return the second elementof the Tuple
getNthElements :: Int -> [(Int, a)] -> [a]
getNthElements n p =
    map snd (filter (\(i, _) -> i `mod` n == 0 ) p)


-- Solution 1b) Using list comprehension
-- Simply get the nth elements for each every position 1..length list
skips_ :: [a] -> [[a]]
skips_ list =
    [getNthElements_ position list | position <- [1..length list] ]


{-
    Get the nth elements from a list.
    We make sure the element always exists by bounding the indices from n-1 to length list - 1
    We increment by n each time
-}
getNthElements_ :: Int -> [a] -> [a]
getNthElements_ n list =
    [list !! i | i <- [n-1, n-1+n..length list - 1]]


-- Exercise 2: Local maxima

{-
    For a local maxima to exist, there must be at least 3 elements.

    A maxima is defined if the middle element is greater than both the left and right elements
    Note that we can recursively call localMaxima on r:xs if a maxima is found, as r can never be
    a maxima if m is maxima.

    If no maxima is found, recursively call localMaxima on m:r:xs
-}
localMaxima :: [Integer] -> [Integer]
localMaxima (l:m:r:xs)
    | l < m && m > r = m : localMaxima (r:xs)
    | otherwise = localMaxima (m:r:xs)
localMaxima _ = []


-- Exercise 3: Histogram


-- Find the # of occurences of each number, generate lines and then append the axis
-- Note: The time complexity would be faster if we used an array, but I guess we should stick w/ lists.
histogram :: [Integer] -> String
histogram xs =
    -- unlines . map (\f -> f) (map generateLine [m,m-1..0]) . count
    unlines (map (generateLine (occurences xs))  [m, m-1..0]) ++ "==========\n0123456789\n"
        where m = maximum $ occurences xs

-- Generate a line using the count list
generateLine :: [Int] -> Int -> String
generateLine occurences lineNum =
    map (\numOccurences -> if numOccurences > lineNum then '*' else ' ' ) occurences


-- Generate a list where the value of each indice holds the count of the number of occurences of each indice
occurences :: [Integer] -> [Int]
occurences xs =
    map (\x -> length (filter (== x) xs )) [0..9]
