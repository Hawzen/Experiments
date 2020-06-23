import Data.List
import Data.Char
import Debug.Trace
import System.Random
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Map (insert)
import qualified Data.Set as Set

data Tree a = Leaf a | Node a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

-- instance Ord (Tree(Int, String)) where
--     compare (Leaf (i, _))       (Leaf (j, _))       = compare i j
--     compare (Leaf (i, _))     n@(Node (j, _) _ _)   = compare i (freq n)
--     compare n@(Node (i, _) _ _)   (Leaf (j, _))       = compare (freq n) j
--     compare n1 n2 = compare (freq n1) (freq n2)


instance Semigroup (Tree (Int, String)) where
    (<>) (Leaf (i, x))     (Leaf (j, y))     = Leaf (i+j, x ++ y)
    (<>) (Leaf (i, x))     (Node (j, y) _ _) = Leaf (i+j, x ++ y)
    (<>) (Node (i, x) _ _) (Leaf (j, y))     = Leaf (i+j, x ++ y)
    (<>) (Node (i, x) _ _) (Node (j, y) _ _) = Leaf (i+j, x ++ y)


instance Monoid (Tree (Int, String)) where  
    mempty = Leaf (0, "")

    orig@(Node (i, x) _ _) `mappend` (Leaf (0, ""))         = orig
    (Leaf (0, ""))         `mappend` orig@(Node (i, x) _ _) = orig

    (Leaf (i, x))     `mappend` (Leaf (j, y))        =
                             Leaf (i+j, x ++ y) 

    (Leaf (i, x))     `mappend` (Node (j, y) l r)    = 
                             Leaf (i+j, x ++ y) `mappend` l `mappend` r

    (Node (i, x) l r) `mappend` (Leaf (j, y))        =
                             Leaf (i+j, x ++ y)  `mappend` l `mappend` r

    (Node (i, x) l r) `mappend` (Node (j, y) l2 r2)  = 
                             Leaf (i+j, x ++ y) `mappend` l `mappend` r
                                                `mappend` l2 `mappend` r2


collectSeperate :: Tree (Int, String) -> String -> [((Int, String), String)]
collectSeperate (Leaf (i, s)) codes = [((i, s), codes)]
collectSeperate node@(Node (i, s) l r) codes = 
                            (collectSeperate l ('L':codes)) ++
                            (collectSeperate r ('R':codes))


collectOne :: Tree (Int, String) -> (Int, String)
collectOne (Leaf (i, s)) = (i, s)
collectOne node@(Node (i, s) l r) = collectOne $ l `mappend` r

freq :: Tree (Int, String) -> Int
freq (Leaf (i, _)) = i
freq node = fst $ collectOne node


stackSort :: [Tree (Int, String)] -> [Tree (Int, String)]
stackSort xs = sortBy (\a b-> compare (freq a) (freq b)) xs


pluck :: [Tree (Int, String)] -> [Tree (Int, String)]
pluck (first:second:xs) = (Node (0, "") first second):xs


pluckAll :: [Tree (Int, String)] -> Tree (Int, String)
pluckAll (fulltree:[]) = fulltree
pluckAll xs = pluckAll $ pluck $ stackSort xs


prob50 :: [(Char, Int)] -> [(Char, String)]
prob50 xs = let stack = map (\(ch,freq) -> Leaf (freq,[ch])) xs
                fulltree = pluckAll stack
             in map (\((i, s:_), code) -> (s, reverse code)) $ 
                                    collectSeperate fulltree ""
