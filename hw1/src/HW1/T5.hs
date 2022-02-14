module HW1.T5 where

import Data.List.NonEmpty as NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep str = NonEmpty.fromList $
    if null str then [[]] else res
        where
            f = \x (cur, acc) -> if x == sep then ([], cur:acc) else (x:cur, acc)
            (finalCur, finalAcc) = foldr f ([], []) str
            res = finalCur : finalAcc


joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep words = joinWith' sep $ NonEmpty.toList words

joinWith' :: a -> [[a]] -> [a]
joinWith' sep [] = []
joinWith' sep [w] = w
joinWith' sep (w:ws) = w ++ (sep : joinWith' sep ws)
