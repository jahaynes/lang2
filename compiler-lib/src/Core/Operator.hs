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

           | EqI
           | LtEqI
           | LtI
           | GtEqI
           | GtI

           | ConcatS
               deriving (Eq, Show)
