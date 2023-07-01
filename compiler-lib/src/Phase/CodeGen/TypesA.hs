module Phase.CodeGen.TypesA where

import Core.Operator
import Core.Types

             -- Simple
data AInstr s = ALabel s
              | AComment s

              | AMov SVal SVal -- to / from
              | ABinOp SVal BinOp SVal SVal -- dest / op / arg1 / arg2

             -- Compound Data
              | Allocate SVal (Allocable s)

             -- Control Flow
              | Push s (Type s) SVal -- debugname / type / idunno?
              | Pop  s (Type s) SVal -- debugname / type / val
              | Call s
              | Ret SVal

                  deriving (Eq, Show)

{- Perhaps String, DataCons, Closure -}
data Allocable s =
    ADataCons (Type s) s
        deriving (Eq, Show)

data SVal = VirtRegPrim !Int
          | VirtRegPtr !Int
          | RLitInt !Integer
             deriving (Eq, Ord, Show)