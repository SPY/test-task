module Deducer where

import Control.Monad.State

import Types

deduce :: [Expression] -> [Maybe Expression]
deduce es = let f e = if hasCycle es e
                      then EEmpty
                      else snd e
                m = map (eval m) (map f (zip [1..] es))
            in m

eval :: [Maybe Expression] -> Expression -> Maybe Expression
eval m EEmpty = Nothing
eval m (ENumber n) = Just $ ENumber n
eval m (EString s) = Just $ EString s
eval m (ERef n) =
  if n > length m
  then Nothing
  else m !! (n - 1)
eval m (EOp o l r) = do
  left <- eval m l
  right <- eval m r
  op o left right

op :: OpType -> Expression -> Expression -> Maybe Expression
op Plus (EString l) (EString r) = Just $ EString $ l ++ r
op Plus (EString l) (ENumber r) = Just $ EString $ l ++ show r
op Plus (ENumber l) (EString r) = Just $ EString $ show l ++ r
op Plus (ENumber l) (ENumber r) = Just $ ENumber $ l + r
op Plus _ _ = Nothing

op Minus (ENumber l) (ENumber r) = Just $ ENumber $ l - r
op Minus _ _ = Nothing

op Mult (ENumber l) (ENumber r) = Just $ ENumber $ l * r
op Mult _ _ = Nothing

op Mod (ENumber l) (ENumber 0) = Nothing
op Mod (ENumber l) (ENumber r) = Just $ ENumber $ l `mod` r
op Mod _ _ = Nothing

op Div (ENumber l) (ENumber 0) = Nothing
op Div (ENumber l) (ENumber r) = Just $ ENumber $ l `div` r
op Div _ _ = Nothing

hasCycle :: [Expression] -> (Int, Expression) -> Bool
hasCycle m (n,e) = hasCycle' e [n]
    where hasCycle' (ERef x) p = if x `elem` p 
                                 then True
                                 else hasCycle' (m !! (x-1)) (n:p)
          hasCycle' EEmpty p = False
          hasCycle' (EString s) _ = False
          hasCycle' (ENumber n) _ = False
          hasCycle' (EOp _ l r) p = hasCycle' l p || hasCycle' r p
