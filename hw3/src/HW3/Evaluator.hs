{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module HW3.Evaluator
  ( eval
  , toHiNumber
  , returnHiList
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import qualified Data.Text as T

import Data.Semigroup (stimes)
import HW3.Base

import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq

import qualified Control.Foldl as L

import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Serialise as SR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time

import Data.Foldable (toList)
import Data.Function (on)
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.Real (Ratio ((:%)))
import Text.Read (readMaybe)

-- | Synonym for 'Evaluation Monad'
type EM m a = ExceptT HiError m a

-- | I'm just a big fan of this video : https://www.youtube.com/watch?v=seVSlKazsNk
infixr 8 ...
(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)


eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr


evalExpr :: HiMonad m => HiExpr -> EM m HiValue

evalExpr (HiExprValue val) = return val

-- | First of all, we check whether @'f'@ applicable or not,
-- then check it's arity and after that
-- try to evaluate it
evalExpr (HiExprApply f args) = do
  f' <- evalExpr f
  unless (isApplicable f') (throwE HiErrorInvalidFunction)
  checkArity f' args
  val <- evalShortCircuit f' args
  maybe (mapM evalExpr args >>= evalApplicable f') return val

evalExpr (HiExprRun expr) = do
  val <- evalExpr expr
  case val of
    (HiValueAction action) -> lift $ runAction action
    _                      -> throwE HiErrorInvalidArgument

-- | 'on' combinator helps to apply same action for two objects - in our case we want to evaluate
-- value for key then value (of course, inside monad context) and then merge them into a pair
evalExpr (HiExprDict kvs) = (HiValueDict . Map.fromList) <$> (mapM (uncurry (liftM2 (,) `on` evalExpr)) kvs)

-- | @'evalShortCircuit'@ returns Just HiValue only for 'And', 'Or', 'If'
-- otherwise return Nothing and we perform evaluations in normal order
evalShortCircuit :: HiMonad m => HiValue -> [HiExpr] -> EM m (Maybe HiValue)

evalShortCircuit (HiValueFunction HiFunIf) [cond, a, b] = do
  cond' <- evalExpr cond
  case cond' of
    (HiValueBool cond'') -> (evalExpr $ if cond'' then a else b) >>= wrapJust
    _                    -> throwE HiErrorInvalidArgument

evalShortCircuit (HiValueFunction HiFunAnd) [a, b] = evalAndOr True a b >>= wrapJust

evalShortCircuit (HiValueFunction HiFunOr) [a, b] = evalAndOr False a b >>= wrapJust

evalShortCircuit _ _ = return Nothing

-- | Yep, copypaste for 'And' / 'Or' - this method helps us
evalAndOr :: HiMonad m => Bool -> HiExpr -> HiExpr -> EM m HiValue
evalAndOr isAnd a b = do
  a' <- evalExpr a
  if isFalseOrNull a' == isAnd then return a' else evalExpr b


wrapJust :: HiMonad m => HiValue -> EM m (Maybe HiValue)
wrapJust = return . Just

isFalseOrNull :: HiValue -> Bool
isFalseOrNull (HiValueBool False) = True
isFalseOrNull HiValueNull = True
isFalseOrNull _ = False


-- | Further I'm using pattern-matching for type-checking - it helps a lot to avoid copy-paste


-- | @'evalApplicable'@ stands for evaluating objects which can be called as functions
-- If we inside this method we supposed that it has correct amount of arguments
-- (because @'checkArity'@ was inside @'evalExpr'@)
evalApplicable :: Monad m => HiValue -> [HiValue] -> EM m HiValue

-- | Function Call
evalApplicable (HiValueFunction f) args = evalFunction f args

-- | String methods (index access, slicing)
evalApplicable (HiValueString s) [(HiValueNumber x)] = index T.length (HiValueString . T.singleton ... T.index) s x
evalApplicable (HiValueString s) [x, y] = slice T.length T.empty (slicer T.take T.drop) HiValueString s x y

-- | List methods (index access, slicing)
evalApplicable (HiValueList sq) [(HiValueNumber x)] = index Seq.length Seq.index sq x
evalApplicable (HiValueList sq) [x, y] = slice Seq.length Seq.empty (slicer Seq.take Seq.drop) HiValueList sq x y

-- | Bytes methods (index access, slicing)
evalApplicable (HiValueBytes bs) [(HiValueNumber x)] = index BS.length (toHiNumber ... BS.index) bs x
evalApplicable (HiValueBytes bs) [x, y] = slice BS.length BS.empty (slicer BS.take BS.drop) HiValueBytes bs x y

-- | Dictionary keys access
evalApplicable (HiValueDict d) [k] = return $ maybe HiValueNull id $ Map.lookup k d

evalApplicable _ _ = throwE HiErrorInvalidArgument


evalFunction :: Monad m => HiFun -> [HiValue] -> EM m HiValue

-- | div
evalFunction HiFunDiv [(HiValueNumber x), (HiValueNumber y)] =  do
  when (y == 0) (throwE HiErrorDivideByZero)
  returnNumber (x / y)

evalFunction HiFunDiv [(HiValueString s1), (HiValueString s2)] = returnStr (s1 <> "/" <> s2)

-- | mul
evalFunction HiFunMul [(HiValueNumber x), (HiValueNumber y)] = returnNumber (x * y)

evalFunction HiFunMul [(HiValueString s), (HiValueNumber x)] = evalMul x s returnStr
evalFunction HiFunMul [x@(HiValueNumber _), s@(HiValueString _)] = evalFunction HiFunMul [s, x]

evalFunction HiFunMul [(HiValueList sq), (HiValueNumber x)] = evalMul x sq returnList
evalFunction HiFunMul [x@(HiValueNumber _), sq@(HiValueList _)] = evalFunction HiFunMul [sq, x]

evalFunction HiFunMul [(HiValueBytes bs), (HiValueNumber x)] = evalMul x bs returnBytes
evalFunction HiFunMul [x@(HiValueNumber _), bs@(HiValueBytes _)] = evalFunction HiFunMul [bs, x]

-- | add
evalFunction HiFunAdd [(HiValueNumber x), (HiValueNumber y)] = returnNumber (x + y)
evalFunction HiFunAdd [(HiValueString s1), (HiValueString s2)] = returnStr (s1 <> s2)
evalFunction HiFunAdd [(HiValueList sq1), (HiValueList sq2)] = returnList (sq1 <> sq2)
evalFunction HiFunAdd [(HiValueBytes bs1), (HiValueBytes bs2)] = returnBytes (bs1 <> bs2)

evalFunction HiFunAdd [(HiValueNumber x), (HiValueTime time)] = returnTime $ Time.addUTCTime (fromRational x) time
evalFunction HiFunAdd [time@(HiValueTime _), x@(HiValueNumber _)] = evalFunction HiFunAdd [x, time]

-- | sub
evalFunction HiFunSub [(HiValueNumber x), (HiValueNumber y)] = returnNumber (x - y)
evalFunction HiFunSub [(HiValueTime t1), (HiValueTime t2)] = returnNumber $ Time.diffUTCTime t1 t2

-- | Logical operations
evalFunction HiFunNot [(HiValueBool x)] = returnBool (not x)
evalFunction HiFunLessThan [a, b] = returnBool (a < b)
evalFunction HiFunGreaterThan [a, b] = returnBool (a > b)
evalFunction HiFunEquals [a, b] = returnBool (a == b)
evalFunction HiFunNotLessThan [a, b] = returnBool (a >= b)
evalFunction HiFunNotGreaterThan [a, b] = returnBool (a <= b)
evalFunction HiFunNotEquals [a, b] = returnBool (a /= b)

evalFunction HiFunIf [(HiValueBool cond), a, b] = return $ if cond then a else b
evalFunction HiFunAnd [a, b] = return $ if isFalseOrNull a then a else b
evalFunction HiFunOr [a, b] = return $ if isFalseOrNull a then b else a

-- | length
evalFunction HiFunLength [(HiValueString s)] = returnNumber (T.length s)
evalFunction HiFunLength [(HiValueList sq)] = returnNumber (Seq.length sq)
evalFunction HiFunLength [(HiValueBytes bs)] = returnNumber (BS.length bs)

-- | String to-upper / to-lower
evalFunction HiFunToUpper [(HiValueString s)] = returnStr (T.toUpper s)
evalFunction HiFunToLower [(HiValueString s)] = returnStr (T.toLower s)

-- | reverse
evalFunction HiFunReverse [(HiValueString s)] = returnStr (T.reverse s)
evalFunction HiFunReverse [(HiValueList sq)] = returnList (Seq.reverse sq)
evalFunction HiFunReverse [(HiValueBytes bs)] = returnBytes (BS.reverse bs)

-- | trim
evalFunction HiFunTrim [(HiValueString s)] = returnStr (T.strip s)

-- | list (...)
evalFunction HiFunList list = returnList' list

-- | range(l, r)
evalFunction HiFunRange [(HiValueNumber l), (HiValueNumber r)] = returnList' $ fmap HiValueNumber [l..r]

-- | fold(a, list)
evalFunction HiFunFold [(HiValueFunction _), (HiValueList Seq.Empty)] = returnNull
evalFunction HiFunFold [(HiValueFunction f), (HiValueList (h :<| t))] = L.foldM folder t
  where folder = L.FoldM (\x a -> evalFunction f [x, a]) (return h) return

-- | Bytes methods

-- | pack
evalFunction HiFunPackBytes [(HiValueList sq)] = HiValueBytes . BS.pack . toList <$> mapM toByte sq
  where
    toByte :: (Monad m) => HiValue -> EM m Word8
    toByte (HiValueNumber (p :% q)) =
      if q == 1 && p >= 0 && p <= 255
      then return $ fromIntegral p
      else throwE HiErrorInvalidArgument
    toByte _ = throwE HiErrorInvalidArgument

-- | unpack
evalFunction HiFunUnpackBytes [(HiValueBytes bs)] = returnList' $ fmap toHiNumber $ BS.unpack bs

-- | encode
evalFunction HiFunEncodeUtf8 [(HiValueString s)] = returnBytes $ Encoding.encodeUtf8 s

-- | decode
evalFunction HiFunDecodeUtf8 [(HiValueBytes bs)] = either (const returnNull) returnStr (Encoding.decodeUtf8' bs)

-- | zip
evalFunction HiFunZip [(HiValueBytes bs)] = returnBytes $ BS.Lazy.toStrict $ compressor $ BS.Lazy.fromStrict bs
  where
    compressor :: BS.Lazy.ByteString -> BS.Lazy.ByteString
    compressor = Zlib.compressWith Zlib.defaultCompressParams { Zlib.compressLevel = Zlib.bestCompression }

-- | unzip
evalFunction HiFunUnzip [(HiValueBytes bs)] = returnBytes $ BS.Lazy.toStrict $ Zlib.decompress $ BS.Lazy.fromStrict bs

-- | serialise
evalFunction HiFunSerialise [x] = returnBytes $ BS.Lazy.toStrict $ SR.serialise x

-- | deserialise
evalFunction HiFunDeserialise [(HiValueBytes bs)] = either (const returnNull) return (deserializer bs)
  where
    deserializer :: BS.ByteString -> Either SR.DeserialiseFailure HiValue
    deserializer = SR.deserialiseOrFail . BS.Lazy.fromStrict

-- | Actions

-- | read
evalFunction HiFunRead [(HiValueString file)] = returnAction $ HiActionRead (T.unpack file)

-- | write
evalFunction HiFunWrite [(HiValueString file), (HiValueString s)] =
  returnAction $ HiActionWrite (T.unpack file) (Encoding.encodeUtf8 s)

evalFunction HiFunWrite [(HiValueString file), (HiValueBytes bs)] =
  returnAction $ HiActionWrite (T.unpack file) bs

-- | mkdir
evalFunction HiFunMkDir [(HiValueString dir)] = returnAction $ HiActionMkDir (T.unpack dir)

-- | chdir
evalFunction HiFunChDir [(HiValueString dir)] = returnAction $ HiActionChDir (T.unpack dir)

-- | parse-time
evalFunction HiFunParseTime [(HiValueString s)] =
  case readMaybe @Time.UTCTime (T.unpack s) of
    (Just time) ->  returnTime time
    Nothing     ->  returnNull

-- | rand
evalFunction HiFunRand [(HiValueNumber l@(l' :% _)), (HiValueNumber r@(r' :% _))] =
  checkIsInt l >> checkIsInt r >> ((returnAction ... HiActionRand `on` fromIntegral) l' r')

-- | echo
evalFunction HiFunEcho [(HiValueString s)] = returnAction $ HiActionEcho s

-- | keys
evalFunction HiFunKeys [(HiValueDict d)] = returnList' $ Map.keys d

-- | values
evalFunction HiFunValues [(HiValueDict d)] = returnList' $ Map.elems d

-- | count
evalFunction HiFunCount [(HiValueList sq)] = return $ count sq id
evalFunction HiFunCount [(HiValueString s)] = return $ count (T.unpack s) (HiValueString . T.singleton)
evalFunction HiFunCount [(HiValueBytes bs)] = return $ count (BS.unpack bs) toHiNumber

-- | invert
evalFunction HiFunInvert [(HiValueDict d)] = returnDict $ fmap returnHiList $ fold $ fmap swap $ Map.toList d
  where
    fold :: [(HiValue, HiValue)] -> Map.Map HiValue [HiValue]
    fold = L.fold (L.foldByKeyMap L.revList)

evalFunction _ _ = throwE HiErrorInvalidArgument

-- | Helpers

-- | Mul
checkMulArg :: (Monad m) => Rational -> EM m ()
checkMulArg (p :% q) = when (p <= 0 || q /= 1) (throwE HiErrorInvalidArgument)

evalMul :: (Monad m, Semigroup c) => Rational -> c -> (c -> EM m HiValue) -> EM m HiValue
evalMul x@(p :% _) c wrapper = checkMulArg x >> wrapper (stimes p c)


-- | toHiNumber
toHiNumber :: (Real a) => a -> HiValue
toHiNumber = HiValueNumber . toRational


-- | Returns

returnNumber :: (Monad m, Real a) => a -> EM m HiValue
returnNumber = return . toHiNumber

returnStr :: Monad m => T.Text -> EM m HiValue
returnStr = return . HiValueString

returnBool :: Monad m => Bool -> EM m HiValue
returnBool = return . HiValueBool

returnNull :: Monad m => EM m HiValue
returnNull = return HiValueNull

returnList :: Monad m => Seq.Seq HiValue -> EM m HiValue
returnList = return . HiValueList

returnHiList :: [HiValue] -> HiValue
returnHiList = HiValueList . Seq.fromList

returnList' :: Monad m => [HiValue] -> EM m HiValue
returnList' = return . returnHiList

returnBytes :: Monad m => BS.ByteString -> EM m HiValue
returnBytes = return . HiValueBytes

returnAction :: Monad m => HiAction -> EM m HiValue
returnAction = return . HiValueAction

returnTime :: Monad m => Time.UTCTime -> EM m HiValue
returnTime = return . HiValueTime

returnDict :: Monad m => Map.Map HiValue HiValue -> EM m HiValue
returnDict = return . HiValueDict

-- | Never forget to check arity!

checkArity :: Monad m => HiValue -> [HiExpr] -> EM m ()
checkArity (HiValueFunction (getFunArity -> (Just expected))) (length -> actual) = throwArityIf (expected /= actual)
checkArity (HiValueString _) args = checkOneTwoArgs args
checkArity (HiValueList _) args = checkOneTwoArgs args
checkArity (HiValueBytes _) args = checkOneTwoArgs args
checkArity (HiValueDict _) args = checkOneTwoArgs args
checkArity _ _                                                 = return ()

checkOneTwoArgs :: Monad m => [HiExpr] -> EM m ()
checkOneTwoArgs (length -> actual) = throwArityIf (actual /= 1 && actual /= 2)

throwArityIf :: Monad m => Bool -> EM m ()
throwArityIf True  = throwE HiErrorArityMismatch
throwArityIf False = return ()


-- | Checks whether we can value as function or not
isApplicable :: HiValue -> Bool
isApplicable (HiValueFunction _) = True
isApplicable (HiValueString _)   = True
isApplicable (HiValueList _)     = True
isApplicable (HiValueBytes _)    = True
isApplicable (HiValueDict _)     = True
isApplicable _                   = False


-- | count method copypaste
count :: (Functor f, Foldable f) => f a -> (a -> HiValue) -> HiValue
count f toHiValue =  HiValueDict $ fmap toHiNumber dict
  where
    dict :: Map.Map HiValue Integer
    dict = L.fold (L.foldByKeyMap L.sum) $ fmap ((,1) . toHiValue) f


-- | Methods for int check

isInt, isNotInt :: Rational -> Bool
isInt n@(_ :% q) = q == 1 && isIntBounded n
isNotInt = not . isInt

isIntBounded :: Rational -> Bool
isIntBounded n = n >= lb && n <= ub
  where (lb, ub) = ((,) `on` toRational) (minBound @Int) (maxBound @Int)

checkIsInt :: Monad m => Rational -> EM m ()
checkIsInt n = when (isNotInt n) (throwE HiErrorInvalidArgument)

-- | index
-- flen - fun to get length
-- findex - to get index
-- c - synonym for collection
--
-- Note: similar naming used for slicing
index :: Monad m => (c -> Int) -> (c -> Int -> HiValue) -> c -> Rational -> EM m HiValue
index flen findex c x@(p :% _) =
  if isNotInt x
  then throwE HiErrorInvalidArgument
  else let i = fromIntegral p in
       if i >= 0 && i < flen c
       then return (findex c i)
       else returnNull

-- | Methods for slicing

-- | Method which return slicer
slicer :: (Int -> c -> c) -> (Int -> c -> c) -> Int -> Int -> c -> c
slicer ftake fdrop l r = ftake (r - l) . fdrop l


slice :: Monad m => (c -> Int) -> c -> (Int -> Int -> c -> c) -> (c -> HiValue) -> c -> HiValue -> HiValue -> EM m HiValue
slice flen empty fslice wrapper c l r = do
  res <- produceSafeSlice l r (flen c)
  return $ wrapper $ case res of
    Nothing         -> empty
    (Just (l', r')) -> fslice l' r' c


produceSafeSlice :: (Monad m) =>  HiValue -> HiValue -> Int -> EM m (Maybe (Int, Int))
produceSafeSlice (HiValueNumber l@(l' :% _)) (HiValueNumber r@(r' :% _)) len  =
  checkIsInt l >> checkIsInt r >> (return $ (produceSlice len `on` fromIntegral) l' r')

produceSafeSlice HiValueNull (HiValueNumber r@(r' :% _)) len  =
  checkIsInt r >> (return $ produceSlice len 0 (fromIntegral r'))

produceSafeSlice (HiValueNumber l@(l' :% _)) HiValueNull len  =
  checkIsInt l >> (return $ produceSlice len (fromIntegral l') len)

produceSafeSlice HiValueNull HiValueNull len  = return $ Just (0, len)

produceSafeSlice _ _ _ = throwE HiErrorInvalidArgument


produceSlice :: Int -> Int -> Int -> Maybe (Int, Int)
produceSlice len l r  = if (l' >= r') then Nothing else Just (max 0 l', min r' len)
  where
    (l', r') = ((,) `on` trueIndex) l r
    trueIndex i = if i < 0 then len - abs i else i
