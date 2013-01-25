module Deducer where

import Control.Monad.State

import Types

deduce :: [Expression] -> [Chunk]
deduce es = let chunks = map Chunk es in
            fst . runState (mapM force chunks) chunks

force :: Chunk -> State [Chunk] Chunk
force (Chunk expr) = do
  case eval expr of
    Nothing -> Corrupted
    Just exp -> Result Expression
force ch         = ch

eval :: Expression -> State [Chunk] (Maybe Expression)
eval EEmpty = Nothing
eval (ENumber n) = ENumber n
eval (EString s) = EString s
eval (ERef n) = do 
  map <- get
  if n > length map
  then Nothing
  else case force $ map !! n of
         Corrupted -> Nothing
         Result e -> Just e
