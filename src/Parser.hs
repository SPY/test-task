module Parser (parseString, showExpressions) where

import Text.Parsec
import Data.List (intercalate)

import Types

plus = char '+' >> return Plus
minus = char '-' >> return Minus
mult = char '*' >> return Mult
divp = string "div" >> return Div
modp = string "mod" >> return Mod

multOp = mult <|> divp <|> modp

factor = try (do
  l <- prim
  spaces
  op <- multOp
  spaces
  r <- prim
  return $ EOp op l r) <|> prim

operation = do
  l <- factor
  spaces
  op <- plus <|> minus
  spaces
  r <- try operation <|> factor
  return $ EOp op l r

op = try operation <|> factor

str = do
  str <- many1 $ noneOf ";+-*"
  return $ EString str

ref = do
  char '$'
  ds <- many1 digit
  return $ ERef $ read ds

num = do
  ds <- many1 digit
  return $ ENumber $ read ds

empty = return EEmpty

prim = (try num) <|> (try ref) <|> str

expr = try op <|> prim

rawExpr = char ';' >> (expr <|> empty)

parseString :: String -> Either ParseError [Expression]
parseString = runParser (many1 rawExpr) ()  "" . (';':)

showExpr :: Maybe Expression -> String
showExpr Nothing = "false"
showExpr (Just (ENumber n)) = show n
showExpr (Just (EString s)) = s

showExpressions = intercalate ";" . map showExpr
