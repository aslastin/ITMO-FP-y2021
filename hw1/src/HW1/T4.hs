module HW1.T4 (tfoldr, treeToList) where
    
import HW1.T3 ( Tree(..) ) 

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf = acc
tfoldr f acc (Branch _ l val r) = tfoldr f (f val (tfoldr f acc r)) l

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
