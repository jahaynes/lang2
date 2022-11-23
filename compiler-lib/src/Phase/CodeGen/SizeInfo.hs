module Phase.CodeGen.SizeInfo where

import Core.Module
import Phase.CodeGen.Val

class Sized a where
    getSize :: a -> Int

newtype SizedVal s =
    SizedVal (Val s)

-- Assumes 64 bits
instance Show s => Sized (SizedVal s) where
    getSize (SizedVal v) =
        case v of
            TypedReg{}    -> 8 -- this is the size of JUST THE REGISTER. good?
            TypedRegPtr{} -> 8 -- this is the size of JUST THE REGISTER. good?
            VDConsName{}  -> 8
            VInt{}        -> 8
            VBool{}       -> 8
            Reg{}         -> 8 -- Guess
            VTag{}        -> 8
            VDCons _name _tag xs -> 8 + sum (map (getSize . SizedVal) xs)

            _            -> error $ "unknown size for: " ++ show v
