module HW3.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Ratio
import Data.Scientific (fromRationalRepetendUnlimited)
import qualified Data.Sequence as S
import Data.Text.Encoding (decodeUtf8)
import HW3.Base
import Numeric (showFFloat)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

-- | Function that return Doc AnsiStyle representation  of HiValue
prettyValue :: HiValue -> Doc AnsiStyle
-- | Number presentation:
-- integers: 42, -8, 15
-- finite decimal fractions: 3.14, -8.15, 77.01
-- fractions: 1/3, -1/7, 3/11
-- mixed fractions: 5 + 1/3, -10 - 1/7, 24 + 3/11
prettyValue (HiValueNumber num)
  | isInt = pretty (quot numPart denPart)
  | isNothing (snd $ fromRationalRepetendUnlimited num) = pretty $ finiteString num
  | otherwise = pretty infiniteString
  where
    finiteString num = showFFloat Nothing (fromRational num) ""
    numPart = numerator num
    denPart = denominator num
    isInt = mod numPart denPart == 0
    signOfPart
      | div (abs numPart) denPart /= 0 = if numPart < 0 then " - " else " + "
      | numPart < 0 = "-"
      | otherwise = ""
    infiniteString = intString ++ signOfPart ++ partString
    intString = let intPart = quot numPart denPart in
      if intPart == 0
      then ""
      else show intPart
    partString = let showNum = mod (abs numPart) denPart in
        show showNum ++ "/" ++ show denPart

-- | Presentation of boolean
prettyValue (HiValueBool v)
  | v = pretty "true"
  | otherwise = pretty "false"

-- | Presentation of string with escaped symbols
prettyValue (HiValueString s) = pretty $ show s

-- | Presentation of HiValueNull
prettyValue HiValueNull = pretty "null"

-- | Presentation of list of values. Each value presented by prettyValue
prettyValue (HiValueList l) = if l == S.empty
  then pretty "[ ]"
  else pretty "[ "
         <> foldl1 (\a b -> a <> pretty ", " <> b) (S.mapWithIndex (\_ el -> prettyValue el) l)
         <> pretty " ]"

-- | Presentation of bytes. Each byte is 2 digit hex-number
prettyValue (HiValueBytes bs) = if bs == B.empty
  then pretty "[# #]"
  else pretty "[# "
    <> pretty (decodeUtf8 $ L.toStrict $ toLazyByteString $
      foldl1 (\a b -> a <> char8 ' ' <> b) (map word8HexFixed (B.unpack bs)))
    <> pretty " #]"

-- | Presentation of name of function
prettyValue (HiValueFunction f) = pretty nameOfF
  where
    nameOfF = case f of
                   HiFunNot            -> "not"
                   HiFunAnd            -> "and"
                   HiFunOr             -> "or"
                   HiFunLessThan       -> "less-than"
                   HiFunGreaterThan    -> "greater-than"
                   HiFunEquals         -> "equals"
                   HiFunNotLessThan    -> "not-less-than"
                   HiFunNotGreaterThan -> "not-greater-than"
                   HiFunNotEquals      -> "not-equals"
                   HiFunSub            -> "sub"
                   HiFunAdd            -> "add"
                   HiFunDiv            -> "div"
                   HiFunMul            -> "mul"
                   HiFunIf             -> "if"
                   HiFunLength         -> "length"
                   HiFunToUpper        -> "to-upper"
                   HiFunToLower        -> "to-lower"
                   HiFunReverse        -> "reverse"
                   HiFunTrim           -> "trim"
                   HiFunList           -> "list"
                   HiFunRange          -> "range"
                   HiFunFold           -> "fold"
                   HiFunPackBytes      -> "pack-bytes"
                   HiFunUnpackBytes    -> "unpack-bytes"
                   HiFunEncodeUtf8     -> "encode-utf8"
                   HiFunDecodeUtf8     -> "decode-utf8"
                   HiFunZip            -> "zip"
                   HiFunUnzip          -> "unzip"
                   HiFunSerialise      -> "serialise"
                   HiFunDeserialise    -> "deserialise"
                   HiFunRead           -> "read"
                   HiFunWrite          -> "write"
                   HiFunMkDir          -> "mkdir"
                   HiFunChDir          -> "cd"
                   HiFunRand           -> "rand"
                   HiFunEcho           -> "echo"
                   HiFunParseTime     -> "parse-time"
                   HiFunCount          -> "count"
                   HiFunKeys           -> "keys"
                   HiFunValues         -> "values"
                   HiFunInvert         -> "invert"

-- | Presentation of actions
prettyValue (HiValueAction act) =
  case act of
       HiActionRead fp     -> pretty $ "read(" ++ show fp ++ ")"
       HiActionMkDir fp    -> pretty $ "mkdir(" ++ show fp ++ ")"
       HiActionChDir fp    -> pretty $ "cd(" ++ show fp ++ ")"
       HiActionCwd         -> pretty "cwd"
       HiActionNow         -> pretty "now"
       HiActionRand l r    -> pretty $ "rand(" ++ show l ++ ", " ++ show r ++ ")"
       HiActionEcho s      -> pretty "echo(" <> prettyValue (HiValueString s) <> pretty ")"
       HiActionWrite fp bs ->
         pretty "write("
         <> viaShow fp
         <> pretty ", "
         <> prettyValue (HiValueBytes bs)
         <> pretty ")"

-- | Presentation of time
prettyValue (HiValueTime t) = pretty $ "parse-time(" ++ show (show t) ++ ")"

-- | Presentation of dictionary of HiValues in keys and values. Each key, value presented by prettyValue
prettyValue (HiValueDict dict) = if dict == M.empty
  then pretty "{ }"
  else pretty "{ "
    <> foldl1 (\a b -> a <> pretty ", " <> b)
      (map (\(a, b) -> prettyValue a <> pretty ": " <> prettyValue b) (M.toList dict))
    <> pretty " }"
