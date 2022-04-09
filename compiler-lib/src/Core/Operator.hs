module Core.Operator where

data UnOp = Negate
          | EShow
          | Err
              deriving (Eq, Show)

data BinOp = AddI
           | SubI

           | MulI
           | DivI
           | ModI

           | EqA
           | LtEqI
           | LtI
           | GtEqI
           | GtI

           | AndB
           | OrB

           | ConcatS
               deriving (Eq, Show)
