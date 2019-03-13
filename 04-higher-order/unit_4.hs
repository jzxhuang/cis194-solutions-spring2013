import           Data.List ((\\))

-- Exercise 1: Wholemeal programming
-- Implement the following using wholemeal programming
fun1:: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1':: [Integer] -> Integer
fun1' =
    product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
    sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else x * 3 + 1)


-- Exercise 2: Folding with trees
-- So this question is for a binary tree, not a binary search tree...
-- Important to insert first, then update height!
-- No idea how to do with a BST!**Incomplete**
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

treeInsert :: a -> Tree a -> Tree a
treeInsert el Leaf = Node 0 Leaf el Leaf
treeInsert el (Node h l v r)
    | treeHeight l < treeHeight r = Node h (treeInsert el l ) v r
    | treeHeight r < treeHeight l = Node h l v (treeInsert el r)
    | otherwise = updateTreeHeight (Node h (treeInsert el l) v r)

treeHeight :: Tree a -> Integer
treeHeight Leaf           = -1
treeHeight (Node h _ _ _) = h

updateTreeHeight :: Tree a -> Tree a
updateTreeHeight Leaf = Leaf
updateTreeHeight (Node _ l v r) =
    Node (1 + max (treeHeight l ) (treeHeight r)) l v r


-- Exercise 3: More folds!
-- 1. Implement a function xor :: [Bool] -> Bool which returns true iff there are an odd # of True values in the list, using fold
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- 2. Implement map as a fold
map':: (a -> b) -> [a] -> [b]
map' f =
    foldr ((:) . f) []

-- 3. Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =
    foldr (flip f) base (reverse xs)


-- Exercise 4: Finding primes (Sieve of Sundaram)
-- Didn't quite get this wihtout fct composition...
sieveSundaram :: Integer -> [Integer]
sieveSundaram =
    map ((+1) . (*2)) . removeNonPrimes

removeNonPrimes :: Integer -> [Integer]
removeNonPrimes n =
    [1..n] \\ sieve n

sieve :: Integer -> [Integer]
sieve n =
    map (\(i, j) -> i + j + 2*i*j) (filter (\(i, j) -> i + j + 2*i*j <= n) (cartProd [1..n] [1..n]))

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys =
    [(x,y) | x <- xs, y <- ys]

