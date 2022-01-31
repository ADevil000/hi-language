{-# LANGUAGE OverloadedStrings #-}
module HW3.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import Data.Word (Word8)
import HW3.Base
import Text.Read (readMaybe)

-- | Data for presentation of type of function. Using for switching in case when evaluates function application.
data TypeOfF =
  TypeArithmeticOp -- | Operations like +,-,*./
  | TypeBool1 -- | 'not' operation
  | TypeLazy2 -- | 'And', 'Or' operations
  | TypeIf -- | 'If' operation
  | TypeCompare -- | Operations like <, >, ==, /= and etc.
  | TypeObjToObj -- | Operations like 'reverse', 'count' and etc.
  | TypeList -- | operation 'list', 'range'.
  | TypeFold -- | 'fold' operation
  | TypeAction -- | HiAction operations
  | TypeTime -- | 'parse-time'
  | TypeDict -- | functions for dictionary

-- | Main function of evaluation of expression. If computation succeeded then return HiValue, else HiError.
-- And result is in HiMonad.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

-- | Helper evaluation function that return ExceptT HiError m HiValue, where m has instance of HiMonad.
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- | Evaluation of HiExprValue. Return content of given expression.
eval' (HiExprValue val) = return val
-- | Evaluation of function application.
-- Check that given expression can be a function and call special helper evaluator
eval' (HiExprApply f list) = do
  fEval <- eval' f
  case fEval of
       HiValueNumber _     -> throwE HiErrorInvalidFunction
       HiValueNull         -> throwE HiErrorInvalidFunction
       HiValueBool _       -> throwE HiErrorInvalidFunction
       HiValueAction _     -> throwE HiErrorInvalidFunction
       HiValueTime _       -> throwE HiErrorInvalidArgument
       HiValueString text  -> computeString text list
       HiValueList l       -> computeList l list
       HiValueFunction fun -> computeWithFun fun list
       HiValueBytes b      -> computeByteString b list
       HiValueDict d       -> computeDict d list
-- | Evaluation of HiExpRun. Execute action and return its result
eval' (HiExprRun expr) = do
  exprEval <- eval' expr
  case exprEval of
       HiValueAction act -> lift $ runAction act
       _                 -> throwE HiErrorInvalidArgument
-- | Evaluation of dictionary creation. Evaluate all key and values and return final version of dictionary
eval' (HiExprDict list) = do
  listE <- mapM (\(k, v) -> do
      kE <- eval' k
      vE <- eval' v
      return (kE, vE)
    ) list
  return $ HiValueDict $ M.fromList listE

-- | Helper function that return arity of given HiFun.
numberOfArgs :: HiFun -> Int
numberOfArgs f = case f of
                      HiFunList           -> -1 -- ^ special check of infinity
                      HiFunAnd            -> 2
                      HiFunOr             -> 2
                      HiFunLessThan       -> 2
                      HiFunGreaterThan    -> 2
                      HiFunEquals         -> 2
                      HiFunNotLessThan    -> 2
                      HiFunNotGreaterThan -> 2
                      HiFunNotEquals      -> 2
                      HiFunSub            -> 2
                      HiFunAdd            -> 2
                      HiFunDiv            -> 2
                      HiFunMul            -> 2
                      HiFunRand           -> 2
                      HiFunRange          -> 2
                      HiFunFold           -> 2
                      HiFunWrite          -> 2
                      HiFunIf             -> 3
                      _                   -> 1

-- | Helper function that return type of given HiFun
typeOfFun :: HiFun -> TypeOfF
typeOfFun f = case f of
                   HiFunDiv            -> TypeArithmeticOp
                   HiFunMul            -> TypeArithmeticOp
                   HiFunAdd            -> TypeArithmeticOp
                   HiFunSub            -> TypeArithmeticOp
                   HiFunNot            -> TypeBool1
                   HiFunAnd            -> TypeLazy2
                   HiFunOr             -> TypeLazy2
                   HiFunLessThan       -> TypeCompare
                   HiFunGreaterThan    -> TypeCompare
                   HiFunEquals         -> TypeCompare
                   HiFunNotLessThan    -> TypeCompare
                   HiFunNotGreaterThan -> TypeCompare
                   HiFunNotEquals      -> TypeCompare
                   HiFunIf             -> TypeIf
                   HiFunLength         -> TypeObjToObj
                   HiFunToUpper        -> TypeObjToObj
                   HiFunToLower        -> TypeObjToObj
                   HiFunReverse        -> TypeObjToObj
                   HiFunTrim           -> TypeObjToObj
                   HiFunList           -> TypeList
                   HiFunRange          -> TypeList
                   HiFunFold           -> TypeFold
                   HiFunPackBytes      -> TypeObjToObj
                   HiFunUnpackBytes    -> TypeObjToObj
                   HiFunEncodeUtf8     -> TypeObjToObj
                   HiFunDecodeUtf8     -> TypeObjToObj
                   HiFunZip            -> TypeObjToObj
                   HiFunUnzip          -> TypeObjToObj
                   HiFunSerialise      -> TypeObjToObj
                   HiFunDeserialise    -> TypeObjToObj
                   HiFunRead           -> TypeAction
                   HiFunWrite          -> TypeAction
                   HiFunMkDir          -> TypeAction
                   HiFunChDir          -> TypeAction
                   HiFunParseTime     -> TypeTime
                   HiFunRand           -> TypeAction
                   HiFunEcho           -> TypeAction
                   HiFunCount          -> TypeObjToObj
                   HiFunKeys           -> TypeDict
                   HiFunValues         -> TypeDict
                   HiFunInvert         -> TypeDict

-- | Function that evaluates searching of value in dictionary by the key
computeDict :: HiMonad m => M.Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
computeDict d [key] = do
  keyEval <- eval' key
  case M.lookup keyEval d of
       Just v  -> return v
       Nothing -> return HiValueNull
computeDict _ _ = throwE HiErrorInvalidArgument

-- | Helper evaluation that compute function application when function is HiFun.
-- Use typeOfFun for choosing correct helper function.
computeWithFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
computeWithFun f list = if (numberOfArgs f /= -1) && (numberOfArgs f /= length list)
  then throwE HiErrorArityMismatch
  else
    case typeOfFun f of
         TypeBool1        -> computeBool1 not list
         TypeLazy2        -> computeLazyAndOR f list
         TypeCompare      -> computeCompare opCompare list
         TypeIf           -> computeIf list
         TypeArithmeticOp -> computeArithmeticOp f list
         TypeObjToObj     -> computeObjToObj f list
         TypeList         -> createList f list
         TypeFold         -> computeFold list
         TypeAction       -> createAction f list
         TypeTime         -> computeTime list
         TypeDict         -> computeFunForDict f list
  where
    -- | Helper case that returns comparing function for HiFun that is TypeCompare, otherwise error
    opCompare = case f of
                     HiFunLessThan       -> (<)
                     HiFunGreaterThan    -> (>)
                     HiFunEquals         -> (==)
                     HiFunNotLessThan    -> (>=)
                     HiFunNotGreaterThan -> (<=)
                     HiFunNotEquals      -> (/=)
                     _                   -> error $ "Impossible function for compare: " ++ show f ++ "."

-- | Function that computes applying function to dictionary
computeFunForDict :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
computeFunForDict f [d] = do
  dictEval <- eval' d
  case dictEval of
       (HiValueDict dict) ->
        case f of
             HiFunKeys   -> return $ HiValueList $ S.fromList $ M.keys dict
             HiFunValues -> return $ HiValueList $ S.fromList $ M.elems dict
             HiFunInvert -> return $ HiValueDict $
               M.map HiValueList (M.fromListWith (S.><) (map (\(a, b) -> (b, S.singleton a)) (M.toList dict)))
             _           -> throwE HiErrorInvalidFunction
       _ -> throwE HiErrorInvalidArgument
computeFunForDict _ _ = throwE HiErrorInvalidArgument

-- | Function that tries to parse time form string
computeTime :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
computeTime [time] = do
  timeEval <- eval' time
  case timeEval of
       HiValueString str -> do
         let parseRes = readMaybe $ T.unpack str
         case parseRes of
              Just timeRes -> return $ HiValueTime timeRes
              Nothing      -> return HiValueNull
       _ -> throwE HiErrorInvalidArgument
computeTime _ = throwE HiErrorInvalidArgument

-- | Function that creates action with filepath and possible argument.
createAction :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
createAction f [val] = do
  valE <- eval' val
  case valE of
       (HiValueString s) -> do
         let str = T.unpack s
         case f of
              HiFunRead  -> return $ HiValueAction $ HiActionRead str
              HiFunMkDir -> return $ HiValueAction $ HiActionMkDir str
              HiFunChDir -> return $ HiValueAction $ HiActionChDir str
              HiFunEcho  -> return $ HiValueAction $ HiActionEcho s
              _          -> throwE HiErrorInvalidFunction
       _ -> throwE HiErrorInvalidArgument
createAction f [arg1, arg2] =
  do
    arg1Eval <- eval' arg1
    arg2Eval <- eval' arg2
    case f of
         HiFunRand ->
           case (arg1Eval, arg2Eval) of
                (HiValueNumber leftBound, HiValueNumber rightBound) ->
                  if not (isInt leftBound && isInt rightBound)
                  then throwE HiErrorInvalidArgument
                  else return $ HiValueAction $ HiActionRand (toInt leftBound) (toInt rightBound)
                _ -> throwE HiErrorInvalidArgument
         HiFunWrite ->
            case (arg1Eval, arg2Eval) of
                 (HiValueString fileName, HiValueString strWrite) -> do
                   let strBytes = encodeUtf8 strWrite
                   let fileNameFP = T.unpack fileName
                   return $ HiValueAction $ HiActionWrite fileNameFP strBytes
                 (HiValueString fileName, HiValueBytes bs)        -> do
                   let fileNameFP = T.unpack fileName
                   return $ HiValueAction $ HiActionWrite fileNameFP bs
                 _                                                -> throwE HiErrorInvalidArgument
         _         -> throwE HiErrorInvalidFunction
createAction _ _ = throwE HiErrorInvalidArgument

-- | Function for computing fold. Use eval for computing each pair.
computeFold :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
computeFold [f, list] = do
  listEval <- eval' list
  case listEval of
       HiValueList (el S.:<| seqTail) -> foldlM foldFun el seqTail
       (HiValueList _)                -> return HiValueNull
       _                              -> throwE HiErrorInvalidArgument
  where
    -- | Helper that evaluates given function with two arguments of type of HiValue
    foldFun :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
    foldFun left right = do
      let leftExpr = HiExprValue left
      let rightExpr = HiExprValue right
      eval' $ HiExprApply f [leftExpr, rightExpr]
computeFold _ = throwE HiErrorArityMismatch

-- | Function that create HiValueList from sequence or range
createList :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
createList f list = do
  listEval <- mapM eval' list;
  case f of
       HiFunList  -> return $ HiValueList $ S.fromList listEval
       HiFunRange ->
         case listEval of
              [HiValueNumber start, HiValueNumber end] ->
                return $ HiValueList $ S.fromList $ map HiValueNumber (rangeRation start end)
              _                                        -> throwE HiErrorInvalidArgument
       _          -> throwE HiErrorInvalidFunction
  where
    -- | Helper function for creating range of rational numbers
    rangeRation :: Rational -> Rational -> [Rational]
    rangeRation a b = [a..b]

-- | Function for evaluating object from one to another. Serialise for every HiValue
-- and special cases for HiValueList, HiValueString, HiValueBytes
computeObjToObj :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
computeObjToObj f [obj] = do
  tVal <- eval' obj
  case f of
       HiFunSerialise -> return $ HiValueBytes $ L.toStrict $ serialise tVal
       _              -> do
         case tVal of
              HiValueString s -> fForString f s
              HiValueList l   -> fForSeq f l
              HiValueBytes bs -> fForBs f bs
              _               -> throwE HiErrorInvalidArgument
  where
    -- | Helper function for unpacking bytes to HiValueList
    unpack :: B.ByteString -> HiValue
    unpack = HiValueList . S.fromList . map (HiValueNumber . toRational . fromEnum) . B.unpack
    -- | Helper function for decoding bytes to string. If decode fail then return HiValueNull
    decode :: B.ByteString -> HiValue
    decode bs = case decodeUtf8' bs of
                     Right v -> HiValueString v
                     Left _  -> HiValueNull
    -- | Function that convert Word8 to HiValueNumber
    bToRat :: Word8 -> HiValue
    bToRat n = HiValueNumber $ toRational n
    -- | Increment function for HiValueNumber
    incNum :: HiValue -> Maybe HiValue
    incNum (HiValueNumber n) = Just $ HiValueNumber $ toRational $ toInt n + 1
    incNum _                 = Nothing
    -- | Compression function with compressLevel that is bestCompression
    compressF :: L.ByteString -> L.ByteString
    compressF = compressWith defaultCompressParams { compressLevel = bestCompression }
    -- | Function for updating dictionary. If key is new for dictionary insert 1, else increment value of key
    addOrUpdate :: HiValue -> M.Map HiValue HiValue -> M.Map HiValue HiValue
    addOrUpdate k m = if M.member k m then M.update incNum k m else M.insert k (HiValueNumber 1) m
    -- | Function that packs char to Text
    chToText :: Char -> HiValue
    chToText ch = HiValueString $ T.pack [ch]
    -- | Function that changes list of HiValuesNumbers that are from 0 to 255 to HiValueBytes presentation
    listToBytestring :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
    listToBytestring (HiValueBytes bs) (HiValueNumber r) = if isInt r && 0 < toInt r && toInt r < 256
      then return $ HiValueBytes $ bs <> B.pack ((\v -> [toEnum v]) $ toInt r)
      else throwE HiErrorInvalidArgument
    listToBytestring _ _ = throwE HiErrorInvalidArgument
    -- | Helper case for getting evaluation function for HiValueBytes
    fForBs f = case f of
      HiFunLength      -> return . HiValueNumber . toRational . B.length
      HiFunCount       -> return . HiValueDict . foldl (\m b -> addOrUpdate (bToRat b) m) M.empty . B.unpack
      HiFunReverse     -> return . HiValueBytes . B.reverse
      HiFunUnpackBytes -> return . unpack
      HiFunDecodeUtf8  -> return . decode
      HiFunZip         -> return . HiValueBytes . L.toStrict . compressF . L.fromStrict
      HiFunUnzip       -> return . HiValueBytes . L.toStrict . decompressWith defaultDecompressParams . L.fromStrict
      HiFunDeserialise -> return . deserialise . L.fromStrict
      _                -> \_ -> throwE HiErrorInvalidArgument
    -- | Helper case for getting evaluation function for HiValueString
    fForString f = case f of
      HiFunLength     -> return . HiValueNumber . toRational . T.length
      HiFunCount      -> return . HiValueDict . foldl (\m ch -> addOrUpdate (chToText ch) m) M.empty . T.unpack
      HiFunReverse    -> return . HiValueString . T.reverse
      HiFunEncodeUtf8 -> return . HiValueBytes . encodeUtf8
      HiFunToUpper    -> return . HiValueString . T.toUpper
      HiFunToLower    -> return . HiValueString . T.toLower
      HiFunTrim       -> return . HiValueString . T.strip
      _               -> \_ -> throwE HiErrorInvalidArgument
    -- | Helper case for getting evaluation function for HiValueList
    fForSeq f = case f of
      HiFunLength    -> return . HiValueNumber . toRational . S.length
      HiFunCount     -> return . HiValueDict . foldl (flip addOrUpdate) M.empty
      HiFunReverse   -> return . HiValueList . S.reverse
      HiFunPackBytes -> foldlM listToBytestring (HiValueBytes B.empty)
      _              -> \_ -> throwE HiErrorInvalidArgument
computeObjToObj _ _ = throwE HiErrorArityMismatch

-- | Function for evaluating arithmetic operations and their overloads
computeArithmeticOp :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
computeArithmeticOp f [left, right] = do
  leftVal <- eval' left
  rightVal <- eval' right
  case (leftVal, rightVal) of
       (HiValueNumber l, HiValueNumber r)   ->
          if isDiv f && r == 0
          then throwE HiErrorDivideByZero
          else return $ HiValueNumber $ opNum l r
       (HiValueTime t, HiValueNumber n)     ->
          if f == HiFunAdd
          then return $ HiValueTime $ addUTCTime (realToFrac n) t
          else throwE HiErrorInvalidArgument
       (HiValueTime t1, HiValueTime t2)     ->
          if f == HiFunSub
          then return $ HiValueNumber $ toRational $ diffUTCTime t1 t2
          else throwE HiErrorInvalidArgument
       (HiValueString s, HiValueNumber n)   -> repeatCommonSeq f stringConstructor s n
       (HiValueBytes bs, HiValueNumber n)   -> repeatCommonSeq f bytesConstructor bs n
       (HiValueList l, HiValueNumber n)     -> repeatCommonSeq f listConstructor l n
       (HiValueString s1, HiValueString s2) ->
          case f of
               HiFunDiv -> return $ HiValueString $ foldl T.append s1 ["/", s2]
               _        -> concatCommonSeq f stringConstructor T.append s1 s2
       (HiValueList l1, HiValueList l2)     -> concatCommonSeq f listConstructor (S.><) l1 l2
       (HiValueBytes bs1, HiValueBytes bs2) -> concatCommonSeq f bytesConstructor B.append bs1 bs2
       _                                    -> throwE HiErrorInvalidArgument
  where
    stringConstructor = return . HiValueString
    bytesConstructor = return . HiValueBytes
    listConstructor = return . HiValueList
    -- | Function for checking that HiFun is division.
    isDiv HiFunDiv = True
    isDiv _        = False
    -- | Case for number arithmetic operations
    opNum = case f of
                 HiFunDiv -> (/)
                 HiFunMul -> (*)
                 HiFunAdd -> (+)
                 HiFunSub -> (-)
                 _        -> error $ "Impossible function for numbers: " ++ show f
computeArithmeticOp _ _ = throwE HiErrorArityMismatch

-- | Function that evaluates concatenation of any sequence objects
concatCommonSeq
  :: HiMonad m
  => HiFun  -- ^ HiFun for checking that it's add overload
  -> (t -> ExceptT HiError m a)  -- ^ Constructor for result of concatenation
  -> (t1 -> t2 -> t)  -- ^ Function for concatenation
  -> t1  -- ^ Left argument
  -> t2  -- ^ Right argument
  -> ExceptT HiError m a  -- ^ Result of concatenation
concatCommonSeq HiFunAdd constr concatF left right = constr $ concatF left right
concatCommonSeq _ _ _ _ _                          = throwE HiErrorInvalidArgument

-- | Function that evaluates repeating og any sequence object
repeatCommonSeq
  :: (Semigroup t, HiMonad m)
  => HiFun  -- ^ HiFun for checking that it's multiply overload
  -> (t -> ExceptT HiError m a)  -- ^ Constructor for result of repeating
  -> t  -- ^ sequence that will be repeated
  -> Rational  -- ^ number of times for repeating
  -> ExceptT HiError m a  -- ^ Result of function
repeatCommonSeq HiFunMul constr seqV n = if isInt n && toInt n > 0
  then constr $ stimes (toIntegerFromRational n) seqV
  else throwE HiErrorInvalidArgument
repeatCommonSeq _ _ _ _ = throwE HiErrorInvalidArgument

-- | Function for evaluating unary bool operations
computeBool1 :: HiMonad m => (Bool -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
computeBool1 f [arg] = do
  argVal <- eval' arg
  case argVal of
       HiValueBool v -> return $ HiValueBool $ f v
       _             -> throwE HiErrorInvalidArgument
computeBool1 _ _ = throwE HiErrorArityMismatch

-- | Function that lazily evaluates HiFunOr and HiFunAnd functions
computeLazyAndOR :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
computeLazyAndOR f [arg1, arg2] = do
  arg1Eval <- eval' arg1
  case f of
       HiFunAnd -> if cond arg1Eval then return arg1Eval else eval' arg2
       HiFunOr  -> if cond arg1Eval then eval' arg2 else return arg1Eval
       _        -> throwE HiErrorInvalidFunction
  where
    -- | Condition that show that left argument is false or null
    cond arg = arg == HiValueBool False || arg == HiValueNull
computeLazyAndOR _ _ = throwE HiErrorArityMismatch

-- | Function that compares all HiValues
computeCompare :: HiMonad m => (HiValue -> HiValue -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
computeCompare f [arg1, arg2] = do
  arg1Val <- eval' arg1
  arg2Val <- eval' arg2
  return $ HiValueBool $ f arg1Val arg2Val
computeCompare _ _ = throwE HiErrorArityMismatch

-- | Function thqt return lazy if that doesn't compute unnecessary argument
computeIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
computeIf [predicate, ifTrue, ifFalse] = do
  predRes <- eval' predicate
  case predRes of
       HiValueBool True  -> eval' ifTrue
       HiValueBool False -> eval' ifFalse
       _                 -> throwE HiErrorInvalidArgument
computeIf _ = throwE HiErrorArityMismatch

-- | Function that evaluates taking by index or slicing of any sequence object
computeSeqObj
  :: HiMonad m
  => (Int -> Int -> t -> HiValue)  -- ^ function for slicing
  -> (t -> Int)  -- ^ function for getting length of sequence
  -> (t -> Int -> ExceptT HiError m HiValue)  -- ^ function for taking by index
  -> (t -> ExceptT HiError m HiValue)  -- ^ function for returning given sequence like HiValue
  -> t  -- ^ sequence
  -> [HiExpr]  -- ^ argument. If on then use for taking by index, if two then slicing, otherwise HiErrorArityMismatch
  -> ExceptT HiError m HiValue  -- ^ Result of computation
computeSeqObj _ lengthF indexF _ seqV [num] = do
  numVal <- eval' num
  case numVal of
       HiValueNumber n ->
         if not (isInt n) then throwE HiErrorInvalidArgument
         else let intN = toInt n in
           if intN < 0 || intN >= lengthF seqV
           then return HiValueNull
           else indexF seqV (toInt n)
       _ -> throwE HiErrorInvalidArgument
computeSeqObj slice lengthF _ idVal seqV [leftBound, rightBound] = do
  leftVal <- eval' leftBound
  rightVal <- eval' rightBound
  case (leftVal, rightVal) of
       (HiValueNumber l, HiValueNumber r) -> if not (isInt l && isInt r)
          then throwE HiErrorInvalidArgument
          else return $ slice (toInt l) (toInt r) seqV
       (HiValueNull, HiValueNumber r) -> if not (isInt r)
          then throwE HiErrorInvalidArgument
          else return $ slice 0 (toInt r) seqV
       (HiValueNumber l, HiValueNull) -> if not (isInt l)
          then throwE HiErrorInvalidArgument
          else return $ slice (toInt l) (lengthF seqV) seqV
       (HiValueNull, HiValueNull) -> idVal seqV
       _ -> throwE HiErrorInvalidArgument
computeSeqObj _ _ _ _ _ _ = throwE HiErrorArityMismatch

-- | Function that compute taking by index or slicing for string
computeString :: HiMonad m => T.Text -> [HiExpr] -> ExceptT HiError m HiValue
computeString = computeSeqObj sliceString T.length indexF (return . HiValueString)
  where
    -- | Helper function for taking by index and returning HiValue in monad
    indexF t i = return $ HiValueString $ T.pack [T.index t i]

-- | Function that compute taking by index or slicing for list
computeList :: HiMonad m => S.Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
computeList = computeSeqObj sliceList S.length (\s i -> return $ S.index s i) (return . HiValueList)

-- | Function that compute taking by index or slicing for bytes
computeByteString :: HiMonad m => B.ByteString -> [HiExpr] -> ExceptT HiError m HiValue
computeByteString = computeSeqObj sliceByteString B.length indexF (return . HiValueBytes)
  where
    -- | Helper function for taking by index and returning HiValue in monad
    indexF s i = return $ HiValueNumber $ toRational $ fromEnum $ B.index s i

-- | Function for making slice on string
sliceString :: Int -> Int -> T.Text -> HiValue
sliceString = commonSlice HiValueString T.take T.drop T.empty T.length

-- | Function for making slice on list
sliceList :: Int -> Int -> S.Seq HiValue -> HiValue
sliceList = commonSlice HiValueList S.take S.drop S.empty S.length

-- | Function for making slice on bytes
sliceByteString :: Int -> Int -> B.ByteString -> HiValue
sliceByteString = commonSlice HiValueBytes B.take B.drop B.empty B.length

-- | Helper function for making slice on any sequnce object
commonSlice
  :: (p -> HiValue)  -- ^ Constructor of HiValue from result of slice
  -> (Int -> t -> p)  -- ^ Function for taking n elements from start of sequence
  -> (Int -> t1 -> t)  -- ^ Function for droping n elements from start of sequence
  -> p  -- ^ Representation of empty sequence
  -> (t1 -> Int)  -- ^ Function for getting length of sequence
  -> Int  -- ^ Left index for slice
  -> Int  -- ^ Right index for slice
  -> t1  -- ^ sequence for slicing
  -> HiValue  -- ^ Result of slicing in HiValue
commonSlice constr takeF dropF empty' lengthF l r list = if lInd < rInd
  then constr $ takeF rangeLR $ dropF lInd list
  else constr empty'
  where
    -- | Length of sequence
    seqLength = lengthF list
    -- | Real left index for slicing
    lInd = if l < 0 then seqLength + l else l
    -- | real right index for slicing
    rInd = if r < 0 then seqLength + r else r
    -- | Number of elements in slice
    rangeLR = if lInd < 0 then rInd else rInd - lInd

-- | Helper function that check that given rational is correct Int
isInt :: Rational -> Bool
isInt r = mod (numerator r) (denominator r) == 0 
  && (toRational (minBound :: Int) <= r) 
  && (r <= toRational (maxBound :: Int))

-- | Helper Function that turn Rational to Integer
toIntegerFromRational :: Rational -> Integer
toIntegerFromRational r = div (numerator r) (denominator r)

-- | Helper Function that turn Rational to Int
toInt :: Rational -> Int
toInt = fromInteger . toIntegerFromRational
