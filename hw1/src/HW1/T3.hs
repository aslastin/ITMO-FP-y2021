module HW1.T3
    ( Tree(..)
    , tsize
    , tdepth
    , tmember
    , tinsert
    , tFromList
    ) where

-- AVL-дерево

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a)

-- Get

getRight, getLeft :: Tree a -> Tree a
getRight (Branch _ _ _ r) = r
getRight _ = undefined

getLeft (Branch _ l _ _) = l
getLeft _ = undefined

-- Update

updateRight :: Tree a -> Tree a -> Tree a
updateRight (Branch (sz, h) l val _) r = Branch (sz, h) l val r
updateRight _ _ = undefined

updateLeft :: Tree a -> Tree a -> Tree a
updateLeft (Branch (sz, h) _ val r) l = Branch (sz, h) l val r
updateLeft _ _ = undefined

-- Implementation

balanceFactor :: Tree a -> Int
balanceFactor (Branch _ l _ r) = tdepth r  - tdepth l
balanceFactor _ = undefined

recalcTree :: Tree a -> Tree a
recalcTree t@(Branch _ l val r) = Branch (1 + sz1 + sz2, 1 + max h1 h2) l val r
    where
        h1 = tdepth l
        h2 = tdepth r
        sz1 = tsize l
        sz2 = tsize r
recalcTree _ = undefined

rotateRight :: Tree a -> Tree a
rotateRight t@(Branch _ l _ r) = recalcTree $ updateRight l $ recalcTree $ updateLeft t $ getRight l
rotateRight _ = undefined

rotateLeft :: Tree a -> Tree a
rotateLeft t@(Branch _ l _ r) = recalcTree $ updateLeft r $ recalcTree $ updateRight t $ getLeft r
rotateLeft _ = undefined

balance :: Tree a -> Tree a
balance t@(Branch _ l _ r)
    | balanceFactor t == 2  = rotateLeft  $ if balanceFactor r < 0 then updateRight t $ rotateRight r else t
    | balanceFactor t == -2 = rotateRight $ if balanceFactor l > 0 then updateLeft t $ rotateLeft l else t
    | otherwise             = t
balance _ = undefined

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize (Branch (sz, _) _ _ _) = sz
tsize Leaf = 0

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth (Branch (_, h) _ _ _) = h
tdepth Leaf = 0

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember el (Branch _ l val r)
    | el == val = True
    | el < val  = tmember el l
    | otherwise = tmember el r

-- | Insert an element into the tree, O(log n)
tinsert, tinsert' :: Ord a => a -> Tree a -> Tree a
tinsert el t = if tmember el t then t else tinsert' el t

tinsert' el Leaf = Branch (1, 1) Leaf el Leaf
tinsert' el t@(Branch _ l val r) =
    balance . recalcTree $ if el < val then updateLeft t $ tinsert el l else updateRight t $ tinsert el r

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
