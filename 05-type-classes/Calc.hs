{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import qualified Data.Map   as M
import           Data.Maybe
import           ExprT
import           Parser
import qualified StackVM

-- (2 + 3) × 4 would be represented by the value
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)


-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b


-- Exercise 2
-- Used fmap, see block comment for implementation using (case... of) expression
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
{- evalStr inputString =
    case parseExp Lit Add Mul inputString of
        Just validInput -> Just (eval validInput)
        Nothing         -> Nothing
-}


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id


-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n
        | n <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit = Mod7 . flip mod 7
    add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- Exercise 5
instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- Some helpers to run the Maybe Program we compiled!
runMaybeProgram :: Maybe StackVM.Program -> Either String StackVM.StackVal
runMaybeProgram = StackVM.stackVM . fromMaybe []

compileAndRun :: String -> Either String StackVM.StackVal
compileAndRun = runMaybeProgram . compile

testProgram = testExp :: Maybe StackVM.Program
runTestProgram = runMaybeProgram testProgram


-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT
    = Lit' Integer
    | Add' VarExprT VarExprT
    | Mul' VarExprT VarExprT
    | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit'
    add = Add'
    mul = Mul'

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- The last argument is the Map from the "caller"
-- Can be written as: add a b vs = fmap (+) (a vs) <*> b vs
instance Expr (M.Map String Integer -> Maybe Integer) where
    -- lit n = \_ -> Just n
    lit n _ = Just n    -- recommended by hlint
    add a b vs = case (a vs, b vs) of
        (Just x, Just y) -> Just (x + y)
        _                -> Nothing
    mul a b vs = case (a vs, b vs) of
        (Just x, Just y) -> Just (x * y)
        _                -> Nothing


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

test6 = do
    print $ withVars [("x", 6)] (add (lit 3) (var "x")) == Just 9
    print $ isNothing (withVars [("x", 6)] (add (lit 3) (var "y")))
    print $ withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x"))) == Just 54
