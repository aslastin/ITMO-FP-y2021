{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HW3.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  , funMap
  , getFunArity
  , getFunName
  ) where

import Data.ByteString.Char8 (ByteString)

import GHC.Generics (Generic)

import Control.Exception.Base (Exception)

import Data.Map (Map, fromList, (!))

import Data.Time.Clock (UTCTime)

import qualified Codec.Serialise as SR
import qualified Data.Sequence as Seq
import qualified Data.Text as T

-- | @'funMap'@ contains all hi-functions's info
--
-- Special methods give access to this information:
-- - @'getFunArity'@ helps to access function's arity (@'Nothing'@ stands for any arity)
-- - @'getFunName'@ gives the name of function in HI language
funMap :: Map HiFun (String, Maybe Int)
funMap = fromList $
  [
    -- T1
    (HiFunDiv, ("div", Just 2)),
    (HiFunMul, ("mul", Just 2)),
    (HiFunAdd, ("add", Just 2)),
    (HiFunSub, ("sub", Just 2)),
    -- T2
    (HiFunNot, ("not", Just 1)),
    (HiFunAnd, ("and", Just 2)),
    (HiFunOr, ("or", Just 2)),
    (HiFunLessThan, ("less-than", Just 2)),
    (HiFunGreaterThan, ("greater-than", Just 2)),
    (HiFunEquals, ("equals", Just 2)),
    (HiFunNotLessThan, ("not-less-than", Just 2)),
    (HiFunNotGreaterThan, ("not-greater-than", Just 2)),
    (HiFunNotEquals, ("not-equals", Just 2)),
    (HiFunIf, ("if", Just 3)),
    -- T4
    (HiFunLength, ("length", Just 1)),
    (HiFunToUpper, ("to-upper", Just 1)),
    (HiFunToLower, ("to-lower", Just 1)),
    (HiFunReverse, ("reverse", Just 1)),
    (HiFunTrim, ("trim", Just 1)),
    -- T5
    (HiFunList, ("list", Nothing)),
    (HiFunRange, ("range", Just 2)),
    (HiFunFold, ("fold", Just 2)),
    -- T6
    (HiFunPackBytes, ("pack-bytes", Just 1)),
    (HiFunUnpackBytes, ("unpack-bytes", Just 1)),
    (HiFunZip, ("zip", Just 1)),
    (HiFunUnzip, ("unzip", Just 1)),
    (HiFunEncodeUtf8, ("encode-utf8", Just 1)),
    (HiFunDecodeUtf8, ("decode-utf8", Just 1)),
    (HiFunSerialise, ("serialise", Just 1)),
    (HiFunDeserialise, ("deserialise", Just 1)),
    -- T7
    (HiFunRead, ("read", Just 1)),
    (HiFunWrite, ("write", Just 2)),
    (HiFunMkDir, ("mkdir", Just 1)),
    (HiFunChDir, ("cd", Just 1)),
    -- T8
    (HiFunParseTime, ("parse-time", Just 1)),
    -- T9
    (HiFunRand, ("rand", Just 2)),
    -- T10
    (HiFunEcho, ("echo", Just 1)),
    -- T11
    (HiFunKeys, ("keys", Just 1)),
    (HiFunValues, ("values", Just 1)),
    (HiFunCount, ("count", Just 1)),
    (HiFunInvert, ("invert", Just 1))
  ]

getFunName :: HiFun -> String
getFunName = fst . (funMap !)

getFunArity :: HiFun -> Maybe Int
getFunArity = snd . (funMap !)


data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Show, Generic, SR.Serialise)


data HiValue
  = HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueNull
  | HiValueString T.Text
  | HiValueList (Seq.Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, SR.Serialise)


data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)


data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Exception)


data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving (Show, Eq, Ord, Generic, SR.Serialise)


class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
