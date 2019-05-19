{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char


newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)


instance Monoid Score where
    mempty = Score 0
    mappend = (+) 


instance Semigroup Score where
    (Score m1) <> (Score m2) = Score (m1 + m2)


score :: Char -> Score
score c 
    | c' `elem` "AEILNORSTU" = Score 1
    | c' `elem` "DG"         = Score 2
    | c' `elem` "BCMP"       = Score 3
    | c' `elem` "FHVWY"      = Score 4
    | c' `elem` "K"          = Score 5
    | c' `elem` "JX"         = Score 8
    | c' `elem` "QZ"         = Score 10
    | otherwise             = Score 0
    where
        c' = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score   -- cool that we can use mconcat!
-- scoreString = foldl' (<>) (Score 0) . map score


-- Helper

getScore :: Score -> Int
getScore (Score i) = i