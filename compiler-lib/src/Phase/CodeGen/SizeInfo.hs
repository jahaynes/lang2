module Phase.CodeGen.SizeInfo where

import Phase.CodeGen.Val

class Sized a where
    getSize :: a -> Int

newtype SizedVal s =
    SizedVal (Val s)

-- Assumes 64 bits
instance Show s => Sized (SizedVal s) where
    getSize (SizedVal v) =
        case v of
            TypedReg{}        -> 8 -- this is the size of JUST THE REGISTER. good?
            TypedRegPtr{}     -> 8 -- this is the size of JUST THE REGISTER. good?
            VDConsNameTyped{} -> 8
            VInt{}            -> 8
            VBool{}           -> 8
            VTag{}            -> 8
            VDConsTyped _type _name _tag xs -> 8 + sum (map (getSize . SizedVal) xs)

            _            -> error $ "unknown size for: " ++ show v
