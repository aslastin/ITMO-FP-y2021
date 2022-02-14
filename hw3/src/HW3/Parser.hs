{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  ( parse
  ) where

import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, empty, eof, label, lookAhead,
                        many, manyTill, notFollowedBy, optional, satisfy, sepBy1, some, takeWhile1P,
                        try, (<?>), (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

import HW3.Base

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Data.Char as Char
import Data.Void (Void)
import Data.Word (Word8)

-- | For our parsing purposes it's enough to use this type
type Parser = Parsec Void String


parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = MP.parse (between spaceConsumer eof hiExpr) ""


hiExpr, hiTerm, hiSubExpr :: Parser HiExpr

-- | Top-level parser of HI language
hiExpr = makeExprParser hiTerm operators

-- | SubExpression which then can have args or dot-calls
hiTerm = hiSubExpr >>= hiTermApplication

hiSubExpr = choice
  [
    between openBr closeBr hiExpr,
    HiExprValue <$> hiValue,
    listBytesP,
    HiExprDict <$> dictP
  ]

hiValue :: Parser HiValue
hiValue = choice
  [
    HiValueNumber <$> numberP,
    HiValueFunction <$> funP,
    HiValueBool <$> boolP,
    HiValueNull <$ keyword "null",
    HiValueString <$> stringP,
    (HiValueAction HiActionCwd) <$ keyword "cwd",
    (HiValueAction HiActionNow) <$ keyword "now"
  ]


numberP :: Parser Rational
numberP = toRational <$> (lexeme $ L.signed spaceConsumer L.scientific) <?> "number"


-- | Automatically creates parsers for all functions listed in @'funMap'@
funP :: Parser HiFun
funP = label "fun" $ choice $ map (\(a, (name, _)) -> a <$ keyword name) $ Map.toDescList funMap


boolP :: Parser Bool
boolP = False <$ keyword "false" <|> True <$ keyword "true" <?> "bool"


stringP :: Parser T.Text
stringP = T.pack <$> (lexeme $ char '"' *> manyTill L.charLiteral (char '"')) <?> "string"


-- | I'm using @'optional (openBr <|> string ".")'@ parser to distinguish function call
-- and applying dot-identifier
hiTermApplication :: HiExpr -> Parser HiExpr
hiTermApplication expr = do
  next <- optional (openBr <|> string ".")
  case next of
      Nothing    -> lexeme $ return expr
      (Just "(") -> app1 >>= hiTermApplication
      _          -> app2 >>= hiTermApplication
   where
     app1 = HiExprApply expr <$> manyExpr <* closeBr
     app2 = (HiExprApply expr . pure . HiExprValue . HiValueString) <$> dotIdentifier


-- | Parser for name after '.'
dotIdentifier :: Parser T.Text
dotIdentifier = (T.pack . List.intercalate "-") <$>
  (((:) <$> satisfy Char.isAlpha <*> many (satisfy Char.isAlphaNum)) `sepBy1` char '-')


-- | If after '[' I'm seeing hash, I suppose that it's bytestring otherwise it's a list and try to parse it
listBytesP :: Parser HiExpr
listBytesP = do
  _ <- openSqBr
  val <- optional hash
  case val of
    Nothing -> wrapFun HiFunList <$> manyExpr <* closeSqBr <?> "list"
    _       -> (HiExprValue . HiValueBytes . BS.pack) <$> many byteP <* hash <* closeSqBr <?> "bytestring"


-- | Parser for single byte which need some comment there:
-- @'lookahead'@ helps me to see how many hex numbers in a row without reading them.
-- It's supposed to be an expensive operation but if we have read more that 2 bytes - we fails that's why
-- we can afford it and then use @'L.hexadecimal'@ to parse two-digit-hex number
byteP :: Parser Word8
byteP = lexeme $ do
  val <- lookAhead (takeWhile1P Nothing Char.isHexDigit)
  if length val /= 2
  then fail "Byte is only a two digit hex number"
  else L.hexadecimal :: Parser Word8

-- | Parser for parsing collections

manyExpr :: Parser [HiExpr]
manyExpr = manyP hiExpr

dictP :: Parser [(HiExpr, HiExpr)]
dictP = between openFigBr closeFigBr $ manyP $ (,) <$> hiExpr <* colon <*> hiExpr

manyP :: Parser a -> Parser [a]
manyP p = (:) <$> p <*> many (comma *> p) <|> mempty


-- | All operators' methods listed there

operators :: [[Operator Parser HiExpr]]
operators =
  [
    [
      postfix "!" HiExprRun
    ],
    [
      binaryLeft "*" HiFunMul,
      binaryLeft "/" HiFunDiv
    ],
    [
      binaryLeft "+" HiFunAdd,
      binaryLeft "-" HiFunSub
    ],
    [
      binaryNon ">=" HiFunNotLessThan,
      binaryNon "<=" HiFunNotGreaterThan,
      binaryNon "<"  HiFunLessThan,
      binaryNon ">"  HiFunGreaterThan,
      binaryNon "/=" HiFunNotEquals,
      binaryNon "==" HiFunEquals
    ],
    [
      binaryRight "&&" HiFunAnd
    ],
    [
      binaryRight "||" HiFunOr
    ]
  ]

-- | With this parser we can afford mutliple '!!!'
postfix :: String -> (HiExpr -> HiExpr) -> Operator Parser HiExpr
postfix name f = Postfix $ foldr1 (.) <$> some (f <$ symbol name)

binaryLeft, binaryRight, binaryNon :: String -> HiFun -> Operator Parser HiExpr
binaryLeft = binary InfixL
binaryRight = binary InfixR
binaryNon = binary InfixN

-- | There were some problems with operatos because '/' is prefix of '/=' but 'notFollowedBy "="' helps to deal with it
binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
binary assoc name f = assoc $ (\a b -> wrapFun f [a, b]) <$ ((lexeme . try) (string name <* notFollowedBy "="))


-- | Middle-level parser helpers

-- | Helper for wrapping functions
wrapFun :: HiFun -> [HiExpr] -> HiExpr
wrapFun = HiExprApply . HiExprValue . HiValueFunction


keyword :: String -> Parser String
keyword = lexeme . string


openBr, closeBr, openSqBr, closeSqBr, comma, hash, openFigBr, closeFigBr, colon :: Parser String
[openBr, closeBr, openSqBr, closeSqBr, comma, hash, openFigBr, closeFigBr, colon] =
  map symbol ["(", ")", "[", "]", ",", "#", "{", "}", ":"]

-- | Low-level parser primitives

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty
