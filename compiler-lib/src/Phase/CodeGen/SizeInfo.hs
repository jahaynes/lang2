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
            RegPtr{}     -> 8
            VDConsName{} -> 8
            VInt{}       -> 8
            Reg{}        -> 8 -- Guess
            VDCons _name _tag xs -> 8 + sum (map (getSize . SizedVal) xs)

            _            -> error $ "unknown size for: " ++ show v
