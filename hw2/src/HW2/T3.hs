module HW2.T3
   ( joinAnnotated
   , joinExcept
   , joinFun
   , joinList
   , joinOption
   )
  where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))

import HW2.T2 (toDefaultList, fromDefaultList)

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some None) = None
joinOption (Some (Some x)) = Some x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error err) = Error err
joinExcept (Success x) = x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# y1) :# y2) = x :# y2 <> y1

joinList :: List (List a) -> List a
joinList list = fromDefaultList $ concat $ [toDefaultList sublist | sublist <- toDefaultList list]

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \x -> let (F g) = f x in g x
