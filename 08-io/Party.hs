module Party where

import Data.Tree
import Employee


-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL xs f) = GL (employee:xs) (f + empFun employee)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs1 f1) (GL xs2 f2) = GL (xs1 ++ xs2) (f1 + f2)

instance Semigroup GuestList where
    m1 <> m2 = mappend m1 m2

-- Can simply use max because GuestList is an instance of Ord
moreFun :: GuestList -> GuestList -> GuestList
-- moreFun x@(GL xs score1) y@(GL ys score2) | score1 < score2 = y
-- moreFun x _ = x
moreFun = max


-- Exercise 2

-- Foldable t => (a -> b -> b) -> b -> t a -> b
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root sf) = f root (map (treeFold f) sf )

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e xs = foldl moreFun mempty (map (\gl -> moreFun gl (glCons e gl)) xs)


-- Exercise 3

-- Can only add boss to the list without the boss, list without bosses is second list
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (GL [boss] (empFun boss), mempty)
nextLevel boss results = (withBoss, withoutBoss)
  where
    withoutBoss = foldMap (uncurry moreFun) results
    withBoss = glCons boss (foldMap snd results)

exercise3 = do
  print $ nextLevel boss guestLists
    where
      boss = Emp "Joe" 5
      guestLists = [(GL [Emp "Stan" 9] 9, GL [Emp "Bob" 3] 3)]


-- Exercise 4

maxFun :: Tree Employee -> GuestList 
maxFun = uncurry moreFun . treeFold nextLevel


-- Exercise 5

main :: IO ()
main = readFile "company.txt" >>= putStrLn . formatGL . maxFun . read

formatGL :: GuestList -> String
formatGL (GL employees fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines (map empName employees)
