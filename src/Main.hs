module Main where

import Parser
import Deducer

main :: IO ()
main = do 
  str <- getLine
  case parseString str of
    Left err -> print err
    Right exprs -> print $ deduce exprs
