module Types where

data Expression = EEmpty
                | EString String
                | ENumber Int
                | EOp OpType Expression Expression
                | ERef Int
                  deriving (Show, Eq)

data OpType = Plus
            | Minus
            | Mult
            | Mod
            | Div
              deriving (Show, Read, Eq)
