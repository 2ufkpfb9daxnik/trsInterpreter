module Parser (readTRSFile) where

import Text.ParserCombinators.Parsec
import TRS

-- Scanners.

uindent :: Parser String
uindent = do
  c <- upper
  s <- many (oneOf "_" <|> alphaNum)
  spaces
  return (c : s)

lindent :: Parser String
lindent = do
  c <- oneOf "_" <|> lower
  s <- many (oneOf "_" <|> alphaNum)
  spaces
  return (c : s)

number :: Parser Int
number = do
  s <- many1 digit
  spaces
  return (read s)

keyword :: String -> Parser ()
keyword s = do
  _ <- string s
  spaces
  return ()

-- Parsing functions.
            
parseTerm :: Parser Term
parseTerm =
  try parseFunction <|>
  try parseVariable <|>
  try parseNumber <|>
  parseList

peano :: Int -> Term
peano 0 = F "0" []
peano n = F "s" [peano (n - 1)]

parseNumber :: Parser Term
parseNumber = do
  n <- number
  return (peano n)

parseVariable :: Parser Term
parseVariable = do
  x <- uindent
  return (V x)

list :: [Term] -> Term
list []       = F "nil" []
list (t : ts) = F "cons" [t, list ts]

parseList :: Parser Term
parseList = do
  keyword "["
  ts <- sepBy parseTerm (keyword ",")
  keyword "]"
  return (list ts)

parseArgs :: Parser [Term]
parseArgs = do
  keyword "("
  ts <- sepBy parseTerm (keyword ",")
  keyword ")"
  return ts

parseFunction :: Parser Term
parseFunction = do
  f <- lindent
  ts <- option [] parseArgs
  return (F f ts)

parseRule :: Parser Rule
parseRule = do
  l <- parseTerm
  keyword "="
  r <- parseTerm
  return (l, r)

parseTRS :: Parser TRS
parseTRS = do
  spaces
  trs <- many parseRule
  eof
  return trs

readTRSFile :: String -> IO (Either ParseError TRS)
readTRSFile path = parseFromFile parseTRS path
