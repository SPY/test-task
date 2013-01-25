module Parser (parseString) where

import Text.Parsec

import Types

plus = char '+' >> return Plus
minus = char '-' >> return Minus
mult = char '*' >> return Mult
divp = string "div" >> return Div
modp = string "mod" >> return Mod

multOp = mult <|> divp <|> modp

simple = try (do
  l <- prim
  spaces
  op <- plus <|> minus
  spaces
  r <- simple
  return $ EOp op l r) <|> prim

factorOp = do
  l <- simple
  spaces
  op <- multOp
  spaces
  r <- expr
  return $ EOp op l r

op = try factorOp <|> simple

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

prim = try num <|> try ref <|> str

expr = try op <|> prim

rawExpr = char ';' >> (expr <|> empty)

parseString :: String -> Either ParseError [Expression]
parseString = runParser (many1 rawExpr) ()  "" . (';':)
