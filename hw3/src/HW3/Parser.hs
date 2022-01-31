module HW3.Parser where

import qualified Control.Monad.Combinators as Comb
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T
import Data.Void
import HW3.Base
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Main function for parsing expressions of language.
-- Return HiExpr in success and ParserError in fail.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (skipSpace *> lexeme exprWithInfixParser <* eof) "Console"

-- | Parser of HiExpr that include infix operators
exprWithInfixParser :: Parser HiExpr
exprWithInfixParser = lexeme (makeExprParser hiExprParser operatorTable) <?> "test"

-- | Type of parser
type Parser = Parsec Void String

-- | Table of infix operators for makeExprParser.
-- Associativity and prioritize operations like in Haskell
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryLeft "*" (createToHiExprF HiFunMul)
    , binaryLeftCheckCont "/" "=" (createToHiExprF HiFunDiv)
    ]
  , [ binaryLeft "+" (createToHiExprF HiFunAdd)
    , binaryLeft "-" (createToHiExprF HiFunSub)
    ]
  , [ binaryNon ">=" (createToHiExprF HiFunNotLessThan)
    , binaryNon "<=" (createToHiExprF HiFunNotGreaterThan)
    , binaryNon "==" (createToHiExprF HiFunEquals)
    , binaryNon "/=" (createToHiExprF HiFunNotEquals)
    , binaryNon "<" (createToHiExprF HiFunLessThan)
    , binaryNon ">" (createToHiExprF HiFunGreaterThan)
    ]
  , [ binaryRight "&&" (createToHiExprF HiFunAnd) ]
  , [ binaryRight "||" (createToHiExprF HiFunOr) ]
  ]

-- | Function that creates HiExprApply for two arguments
createToHiExprF :: HiFun -> HiExpr -> HiExpr -> HiExpr
createToHiExprF f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

-- | Function that create binary left associative Operator for OperatorTable with checking
-- that it isn't followed by special sting
binaryLeftCheckCont :: String -> String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryLeftCheckCont name notFollow f = InfixL (try $ f <$ lexeme (string name <* notFollowedBy (string notFollow)))

-- | Function that create binary left-associative Operator for OperatorTable
binaryLeft :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryLeft name f = InfixL (f <$ lexeme (string name))

-- | Function that create binary right-associative Operator for OperatorTable
binaryRight :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryRight name f = InfixR (f <$ lexeme (string name))

-- | Function that create binary non-associative Operator for OperatorTable
binaryNon :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryNon name f = InfixN (f <$ lexeme (string name))

-- | Parser for space consuming
skipSpace :: Parser ()
skipSpace = L.space space1 empty empty <?> "skip"

-- | Function that add given parser space consuming
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

-- | Parser of standard language expression. There are all possible starts of expression
hiExprParser :: Parser HiExpr
hiExprParser = lexeme (choice
  [ (\a b -> b a) <$> between (lexeme $ char '(') (lexeme $ char ')') exprWithInfixParser
      <*> (foldr (\a b -> b . a) id <$> many hiExprContinuationParser)
  , (\a b -> b (HiExprValue a)) <$> hiValueParser <*> (foldr (\a b -> b . a) id <$> many hiExprContinuationParser)
  , (\a b -> b a) <$> listParser <*> (foldr (\a b -> b . a) id <$> many hiExprContinuationParser)
  , (\a b -> b a) <$> dictParser <*> (foldr (\a b -> b . a) id <$> many hiExprContinuationParser)
  ]) <?> "expr"

-- | Parser of language expression continuation. Result is a function that waits for an expression to apply the parsed
hiExprContinuationParser :: Parser (HiExpr -> HiExpr)
hiExprContinuationParser = lexeme (choice
  [ flippedApply
      <$> between (lexeme $ char '(') (lexeme $ char ')') (sepBy exprWithInfixParser (lexeme $ char ','))
  , (\_ b -> flippedApply [b])
        <$> char '.'
          <*> (HiExprValue . HiValueString . T.pack . foldl1 (\a b -> a ++ "-" ++ b)
            <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'))
  , HiExprRun <$ char '!'
  ]) <?> "exprHelper"
  where
    flippedApply :: [HiExpr] -> HiExpr -> HiExpr
    flippedApply = flip HiExprApply

-- | Parser of dictionary such as a function for evaluation
dictParser :: Parser HiExpr
dictParser = lexeme
  (HiExprDict
    <$> between (lexeme $ char '{') (lexeme $ char '}') (sepBy keyValParser (lexeme (char ','))))
  <?> "dictPaser"

-- | Parser of key value pair for dictionary. Key and value are expressions
keyValParser :: Parser (HiExpr, HiExpr)
keyValParser = lexeme ((,) <$> exprWithInfixParser <* lexeme (char ':') <*> exprWithInfixParser) <?> "keyValParser"

-- | Parser of leaves of expressions - HiValues.
hiValueParser :: Parser HiValue
hiValueParser = lexeme (choice
  [ HiValueNumber <$> rationalParser
  , HiValueNull <$ string "null"
  , HiValueAction HiActionCwd <$ string "cwd"
  , HiValueAction HiActionNow <$ string "now"
  , HiValueFunction <$> hiFunParser
  , HiValueBool <$> boolParser
  , HiValueString . T.pack <$> textParser
  , HiValueBytes <$> bytesParser
  ])
  <?> "value"

-- | Parser of array of bytes
bytesParser :: Parser B.ByteString
bytesParser = (\l -> if l /= [] then foldl1 (<>) l else B.empty)
  <$> lexeme (between (lexeme $ string "[#") (lexeme $ string "#]") (many hexNum)) <?> "byteString"

-- | Parser of one byte that is 2 digit hex-number
hexNum :: Parser B.ByteString
hexNum = B.pack . (\a -> [toEnum a]) . read . ("0x" ++)
  <$> lexeme (Comb.count 2 hexDigitChar <* notFollowedBy hexDigitChar) <?> "byte"

-- | Parser of list of expressions
listParser :: Parser HiExpr
listParser = lexeme
  (HiExprApply (HiExprValue $ HiValueFunction HiFunList)
    <$> between (lexeme $ char '[') (lexeme $ char ']') (sepBy exprWithInfixParser (lexeme (char ','))))
  <?> "listPaser"

-- | Parser of string literals
textParser :: Parser String
textParser = (char '"' >> manyTill L.charLiteral (char '"')) <?> "textParser"

-- | Parser of bool values
boolParser :: Parser Bool
boolParser = lexeme (choice
  [ True <$ string "true"
  , False <$ string "false"
  ])
  <?> "bool"

-- | Parser of numbers
rationalParser :: Parser Rational
rationalParser = lexeme (L.signed skipSpace (lexeme $ toRational <$> L.scientific)) <?> "num"

-- | Parser of function names
hiFunParser :: Parser HiFun
hiFunParser = lexeme (choice
  [ HiFunAdd <$ string "add"
  , HiFunNotLessThan <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals <$ string "not-equals"
  , HiFunSub <$ string "sub"
  , HiFunMul <$ string "mul"
  , HiFunDiv <$ string "div"
  , HiFunAnd <$ string "and"
  , HiFunOr <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , HiFunNot <$ string "not"
  , HiFunIf <$ string "if"
  , HiFunLength <$ string "length"
  , HiFunToUpper <$ string "to-upper"
  , HiFunToLower <$ string "to-lower"
  , HiFunReverse <$ string "reverse"
  , HiFunTrim <$ string "trim"
  , HiFunList <$ string "list"
  , HiFunRange <$ string "range"
  , HiFunFold <$ string "fold"
  , HiFunPackBytes <$ string "pack-bytes"
  , HiFunUnpackBytes <$ string "unpack-bytes"
  , HiFunEncodeUtf8 <$ string "encode-utf8"
  , HiFunDecodeUtf8 <$ string "decode-utf8"
  , HiFunZip <$ string "zip"
  , HiFunUnzip <$ string "unzip"
  , HiFunSerialise <$ string "serialise"
  , HiFunDeserialise <$ string "deserialise"
  , HiFunRead <$ string "read"
  , HiFunWrite <$ string "write"
  , HiFunMkDir <$ string "mkdir"
  , HiFunChDir <$ string "cd"
  , HiFunParseTime <$ string "parse-time"
  , HiFunRand <$ string "rand"
  , HiFunEcho <$ string "echo"
  , HiFunCount <$ string "count"
  , HiFunKeys <$ string "keys"
  , HiFunValues <$ string "values"
  , HiFunInvert <$ string "invert"
  ])
  <?> "function"
