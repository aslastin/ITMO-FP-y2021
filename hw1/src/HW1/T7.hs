module HW1.T7 where

-- ListPlus

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
    (x :+ y) <> z = x :+ (y <> z)
    (Last x) <> z = x :+ z

-- Inclusive

data Inclusive a b = This a | That b | Both a b

instance (Semigroup i, Semigroup j) => Semigroup (Inclusive i j) where
    (This a) <> (This b) = This (a <> b)
    (This a) <> (That b) = Both a b
    (This a) <> (Both b c) = Both (a <> b) c

    (That a) <> (This b) = Both b a
    (That a) <> (That b) = That (a <> b)
    (That a) <> (Both b c) = Both b (a <> c)

    (Both a b) <> (This c) = Both (a <> c) b
    (Both a b) <> (That c) = Both a (b <> c)
    (Both a b) <> (Both c d) = Both (a <> c) (b <> d)

-- DotString

newtype DotString = DS String

instance Semigroup DotString where
    (DS str1) <> (DS str2)
        | str1 == mempty = DS str2
        | str2 == mempty = DS str1
        | otherwise      = DS $ str1 ++ "." ++ str2
    

instance Monoid DotString where
    mempty = DS mempty

-- Fun

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
    (F f1) <> (F f2) = F $ f1 . f2

instance Monoid (Fun a) where
    mempty = F id
