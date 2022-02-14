module HW1.T6 (mcat, epart) where


mcat :: Monoid a => [Maybe a] -> a
mcat = foldr mcat' mempty

mcat' :: Monoid a => Maybe a -> a  -> a
mcat' Nothing acc  = acc
mcat' (Just x) acc  = x <> acc


epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr epart' (mempty, mempty)

epart' :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
epart' (Left a) (acc_a, acc_b)  = (a <> acc_a, acc_b)
epart' (Right b) (acc_a, acc_b)  = (acc_a, b <> acc_b)
