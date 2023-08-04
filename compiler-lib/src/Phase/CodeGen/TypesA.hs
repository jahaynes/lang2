module Phase.CodeGen.TypesA where

import Core.Operator
import Core.Types

             -- Simple
data AInstr s = ALabel s
              | AComment s

              | AMov SVal SVal -- to / from
              | AMovToPtrOff SVal Int SVal -- destptr / offbytes / arg1 -- todo split arg1 for efficiency?
              | AMovFromPtrOff SVal Int SVal -- destreg / offbytes / arg1
              | ABinOp SVal BinOp SVal SVal -- dest / op / arg1 / arg2
              | ACmpB SVal -- Compare its value to 'True'.  Result is directly set in machine.

             -- Compound Data
              | Allocate SVal Int -- dest / bytes

             -- Control Flow
              | Push s (Type s) SVal -- debugname / type / idunno?
              | Pop  s (Type s) SVal -- debugname / type / val
              | Call s
              | Ret SVal
              | J s
              | Je s
              | Jne s

              | AErr s

                  deriving (Eq, Show)

-- TODO split into lvalues/rvalues?
-- clean up the uses in machinea and go from there
data SVal = VirtRegPrim !Int
          | VirtRegPtr !Int
          | RLitBool !Bool
          | RLitInt !Integer
          | RMemAddress !Int
             deriving (Eq, Ord, Show)
