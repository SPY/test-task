module Deducer where

import Control.Monad.State

import Types

deduce :: [Expression] -> [Chunk]
deduce = map (eval . Chunk)

eval :: [Chunk] -> Chunk -> Chunk
eval m EEmpty = Corrupted
eval m e@(EString _) = Result e
eval m e@(ENumber _) = Just e
