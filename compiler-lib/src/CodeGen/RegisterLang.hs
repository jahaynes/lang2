module CodeGen.RegisterLang where

-- A virtual register
newtype R =
    R Int

data Inst s = Push !R
            | Pop !R
            | BinOp !R !DBinOp !R !R

data DBinOp = Plus
            | Minus
            | Times
            | Div
            | Mod
            | Eq
            | And
            | Or
            | Lt
