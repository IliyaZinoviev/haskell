module BinTree where
import Prelude (Int, Char, String, Show(..), (++), Eq(..), Ord(..), Ordering(..))
import Nat

data BinTree a = Leaf a | BinTree (BinTree a) (BinTree a)
    deriving (Show)

reverse :: BinTree a -> BinTree a
reverse (BinTree a b) = BinTree b a

depth :: BinTree a -> Nat
depth (Leaf a) = Succ Zero
depth (BinTree a b) = Succ (if n1 > n2
                            then n1
                            else n2)
    where n1 = depth a
          n2 = depth b

leaves :: BinTree a -> [a]
leaves (Leaf a) = [a]
leaves (BinTree a b) = leaves a ++ leaves b
