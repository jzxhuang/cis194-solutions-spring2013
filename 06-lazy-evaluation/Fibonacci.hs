-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Infinite list of all Fibonacci numbers, fibs1
fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2
-- Basic Idea: need to use previous values
-- Refered to https://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell

fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

-- Much prefer this solution...

fibo :: Integer -> Integer -> [Integer]
fibo a b = a:fibo b (a+b)

fibs3 :: [Integer]
fibs3 = fibo 0 1


-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs
    
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList
    
stream1 :: Stream Integer
stream1 = Cons 1 stream1

streamA :: Stream Char
streamA = Cons 'A' streamA


-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs) 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))


-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Tricky... I understand interleave, but didn't see the pattern!
-- ruler function: 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,... "Starting at n = 1, the highest power of 2 dividing n"
-- Implement by interleaving two streams, 0... and 	1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5, 1, 2
-- But the second stream is just "interleave 1 and the third stream"
-- And the third stream is just "interleave 2 and the fourth stream... etc"
ruler :: Stream Integer
ruler = startRuler 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) y = Cons x (interleaveStreams y xs) 

startRuler :: Integer -> Stream Integer
startRuler n = interleaveStreams (streamRepeat n) (startRuler (n + 1))


-- TO-DO later
-- Exercise 6 (optional)


-- Exercise 7 (optional)
