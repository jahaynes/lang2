module Phase.CodeGen.TypesA where

import Core.Operator
import Core.Types

data AInstr s = ALabel s
              | AComment s

              | J s
              | Je s
              | Jne s

              | AErr s
              
              | Call s
              | Ret AVal
              | Push s (Type s) AVal -- debugname / type / Reg or literal
              | Pop s (Type s) Int -- debugname / type / AReg 

              | AMov MovMode

                -- x86 adds are only 2-param, so this will need later translation
              | ABinOp Int BinOp (Type s) AVal (Type s) AVal -- dest / op / type a / a / type b / b

              | ACmpB AVal -- Compare its value to 'True'.  Result is directly set in machine.

              | Allocate Int Int -- destreg / bytes

                  deriving Show

data MovMode = RegFromReg !Int !Int             --  mov ebx         , eax
             | MemFromReg !Int !Int !Int        --  mov [ebx + 0x10], eax
             | MemFromLitBool !Int !Int !Bool   --  mov [ebx + 0x10], false
             | MemFromLitInt !Int !Int !Integer --  mov [ebx + 0x10], 0x20
             | RegFromMem !Int !Int !Int        --  mov ebx         , [eax + 0x10]
             | RegFromLitInt !Int !Integer      --  mov ebx         , 0x10
             | RegFromLitBool !Int !Bool        --  mov ebx         , true
                 deriving Show

-- I probably can't switch on different AVals during runtime
-- Expand the instructions instead
data AVal = ALitInt !Integer
          | ALitBool !Bool
          | AReg !Int
          | MemAddress !Int
          | AUnkn
              deriving Show
