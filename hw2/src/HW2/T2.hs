module HW2.T2
   ( -- * dist functions
     distAnnotated
   , distExcept
   , distFun
   , distList
   , distOption
   , distPair
   , distPrioritised
   , distQuad
   , distStream
     -- * wrap functions
   , wrapAnnotated
   , wrapExcept
   , wrapFun
   , wrapList
   , toDefaultList
   , fromDefaultList
   , wrapOption
   , wrapPair
   , wrapPrioritised
   , wrapQuad
   , wrapStream
   )
  where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))
import Prelude (Monoid, Semigroup, mempty, (<>), ($), (.), foldr)

{-Hlint ignore "Use newtype instead of data"-}
{-Hlint ignore "Use const"-}

import HW2.T1

wrapOption :: a -> Option a
wrapOption = Some

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some x, Some y) = Some (x, y)
distOption (_, _) = None


wrapPair :: a -> Pair a
wrapPair x = P x x

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 x2, P y1 y2) = P (x1, y1) (x2, y2)


wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)


wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# e1, y :# e2) = (x, y) :# (e1 <> e2)


wrapExcept :: a -> Except e a
wrapExcept = Success

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success x, Success y) = Success (x, y)
distExcept (Error e1, _) = Error e1
distExcept (_, Error e2) = Error e2


wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low x, Low y) = Low (x, y)
distPrioritised (Low x, Medium y) = Medium (x, y)
distPrioritised (Low x, High y) = High (x, y)
distPrioritised (Medium x, Low y) = Medium (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (Medium x, High y) = High (x, y)
distPrioritised (High x, Low y) = High (x, y)
distPrioritised (High x, Medium y) = High (x, y)
distPrioritised (High x, High y) = High (x, y)


wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (h1 :> t1, h2 :> t2) = (h1, h2) :> distStream (t1, t2)


wrapList :: a -> List a
wrapList x = x :. Nil

toDefaultList :: List a -> [a]
toDefaultList Nil = []
toDefaultList (h :. t) = h : toDefaultList t

fromDefaultList :: [a] -> List a
fromDefaultList = foldr (:.) Nil

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (l1, l2) = fromDefaultList $ [(x, y) | x <- toDefaultList l1, y <- toDefaultList l2]


wrapFun :: a -> Fun i a
wrapFun x = F $ \_ -> x

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F $ \x -> (f1 x, f2 x)
