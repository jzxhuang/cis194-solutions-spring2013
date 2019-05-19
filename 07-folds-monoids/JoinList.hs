{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module JoinList where

import Data.Monoid

import Buffer
import Editor
import Scrabble
import Sized


-- m stores monoidal representation, a stores the value
data JoinList m a 
    = Empty
    | Single m a 
    | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y
-- (+++) x y = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


-- Exercise 2

-- Here, the annotation caches the size of the subtree where size is the # of data elements of the subtree
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ x)
    | i == 0 = Just x
    | otherwise = Nothing
indexJ i (Append m l r) 
    | i > getSize (size m) = Nothing
    | i < jlGetSize l = indexJ i l
    | otherwise = indexJ (i - jlGetSize l) r


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i list
    | i <= 0 = list
    | i >= jlGetSize list = Empty
dropJ _ Empty = Empty
dropJ i (Single _ _) = Empty -- no check for i = 0 needed as it's covered earlier
dropJ i (Append m l r) 
    | i >= jlGetSize l = dropJ (i - jlGetSize l) r
    | otherwise = dropJ i l +++ r


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i list 
    | i <= 0 = Empty
    | i >= jlGetSize list = list
takeJ _ Empty = Empty
takeJ _ list@(Single _ _) = list
takeJ i list@(Append m l r)
    | i > jlGetSize l = takeJ i l +++ takeJ (jlGetSize list - i) r      -- take all of left, so can just pass i
    | otherwise = takeJ i l


-- Helper to get size of a join list
jlGetSize :: (Sized m, Monoid m) => JoinList m a -> Int
jlGetSize = getSize . size . tag


-- Testing

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)


a = Append (Size 3)
      (Append (Size 2)
        (Single (Size 1) "hi")
        (Single (Size 1) "bye")
      )
     (Single (Size 1) "tschau")

b = Single (Size 1) "blub"

c = Append (Size 2)
      (Single (Size 1) "hi")
      (Single (Size 1) "bye")

d = Empty

e = Append (Size 1) 
        Empty
        (Append (Size 1)
            Empty 
            (Single (Size 1) "hi")
        )


-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str


-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
    -- | Convert a buffer to a String.
    toString = unlines . jlToList

    -- | Create a buffer from a String.
    -- Not explicit, but makes sense to store each line in a node
    fromString = foldl f Empty . lines
        where
            f jl str = jl +++ Single (scoreString str, 1) str

    -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
    -- for out-of-bounds indices.
    line = indexJ 

    -- | @replaceLine n ln buf@ returns a modified version of @buf@,
    --   with the @n@th line replaced by @ln@.  If the index is
    --   out-of-bounds, the buffer should be returned unmodified.
    replaceLine n str jl 
        | n < 0 || n >= jlGetSize jl = jl
        -- Careful for 0-indexing!
        | otherwise = takeJ n jl +++ fromString str +++ dropJ (n+1) jl

    -- | Compute the number of lines in the buffer.
    numLines = jlGetSize
    
    value = getScore . fst . tag


-- Run the editor!
main = runEditor editor (fromString defaultText :: JoinList (Score, Size) String)

defaultText :: String
defaultText = 
    unlines 
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file."
    ] 