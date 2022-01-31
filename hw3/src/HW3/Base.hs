{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module HW3.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | Data that represent function in language
data HiFun =
    HiFunNot  -- ^ Boolean `not` function
  | HiFunAnd  -- ^ Lazy `and` function
  | HiFunOr  -- ^ Lazy `or` function
  | HiFunLessThan  -- ^ Comparing function (<)
  | HiFunGreaterThan  -- ^ Comparing function (>)
  | HiFunEquals  -- ^ Comparing function (==)
  | HiFunNotLessThan  -- ^ Comparing function (>=)
  | HiFunNotGreaterThan  -- ^ Comparing function (<=)
  | HiFunNotEquals  -- ^ Comparing function (/=)
  | HiFunSub   -- ^ Function of subtraction
  | HiFunAdd  -- ^ Function of adding or concatenation
  | HiFunDiv  -- ^ Division number function or path appending for strings
  | HiFunMul  -- ^ Multiply fucntion
  | HiFunIf  -- ^ Lazy if
  | HiFunLength  -- ^ Function of length of object
  | HiFunToUpper  -- ^ to-upper for strings
  | HiFunToLower  -- ^ to-lower for strings
  | HiFunReverse  -- ^ reverse any sequence
  | HiFunTrim  -- ^ trim spaces from string
  | HiFunList  -- ^ create list of values
  | HiFunRange  -- ^ range of numbers
  | HiFunFold  -- ^ fold of given list of values with given binary function
  | HiFunPackBytes  -- ^ function that return bytes from sequence of valid numbers
  | HiFunUnpackBytes  -- ^ function that return number representation of bytes
  | HiFunEncodeUtf8  -- ^ change given string ot bytes representation
  | HiFunDecodeUtf8  -- ^ try to decode given bytes to string
  | HiFunZip  -- ^ compress given bytes
  | HiFunUnzip  -- ^ decompress given bytes
  | HiFunSerialise  -- ^ serialise any object
  | HiFunDeserialise  -- ^ try deserialise bytes to valid object
  | HiFunRead  -- ^ function for reading from file or directory
  | HiFunWrite  -- ^ function for writing to file
  | HiFunMkDir  -- ^ function for creating new directories
  | HiFunChDir  -- ^ function for changing current directory
  | HiFunParseTime  -- ^ function that try parse time from string
  | HiFunRand  -- ^ function of generating of random number from lowerBound to upperBound (both inclusive)
  | HiFunEcho  -- ^ print given string to console
  | HiFunCount  -- ^ create dictionary form given sequence
  | HiFunKeys  -- ^ return list of keys from dictionary (sorted)
  | HiFunValues  -- ^ return list of values from dictionary (sorted by key)
  | HiFunInvert  -- ^ from dictionary of (keyDict1: valDict1) create dictionary of (valDict1 : [keyDict1])
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Data that represent values of language. It's leaves of epxressions
data HiValue =
    HiValueNull  -- ^ Null value
  | HiValueBool Bool  -- ^ Value for bool representation
  | HiValueNumber Rational  -- ^ Value for number representation
  | HiValueBytes ByteString  -- ^ Value for bytes representation
  | HiValueString T.Text  -- ^ Value for string representation
  | HiValueList (Seq HiValue)  -- ^ Value for list of values representation
  | HiValueFunction HiFun  -- ^ Value for function name representation
  | HiValueAction HiAction  -- ^ Value for action representation
  | HiValueTime UTCTime  -- ^ Value for time representation
  | HiValueDict (Map HiValue HiValue)  -- ^ Value for dictionary representation
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Data that represent expressions of language
data HiExpr =
    HiExprValue HiValue  -- ^ Expression that include only value
  | HiExprApply HiExpr [HiExpr]  -- ^ Expression that present function application
  | HiExprDict [(HiExpr, HiExpr)]  -- ^ Expression that present function for dictionary creation
  | HiExprRun HiExpr  -- ^ Expression for actions that should be run
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Errors that can be thrown by evaluator
data HiError =
  HiErrorInvalidFunction  -- ^ Error occurs than given invalid function for function application
  | HiErrorArityMismatch  -- ^ Error occurs than number of arguments mismatched with function arity
  | HiErrorInvalidArgument  -- ^ Error occurs than given invalid arguments for function
  | HiErrorDivideByZero  -- ^ Error occurs that division by zero happened
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Data that represent valid actions that can be computed by HiMonad
data HiAction =
    HiActionRead  FilePath  -- ^ Read content of file or directory. Need AllowRead permission
  | HiActionWrite FilePath ByteString  -- ^ Write bytes into file. Need AllowWrite permission
  | HiActionMkDir FilePath  -- ^ Create new directory with given filepath. Need AllowWrite permission
  | HiActionChDir FilePath  -- ^ Cahnge current directory. Need AllowRead permission
  | HiActionCwd  -- ^ Return filepath of current directory. Need AllowRead permission
  | HiActionNow  -- ^ Return current time. Need AllowTime permission
  | HiActionRand Int Int  -- ^ Return random number from lower to upper bound (both inclusive).
  | HiActionEcho T.Text  -- ^ Print given string to console. Need AllowWrite permission
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

-- | Class HiMonad for HiAction activation
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
