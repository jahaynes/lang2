module Phase.CodeGen.Val where

import Core.Types
import Data.ByteString (ByteString)

data Val s = TypedReg (Type s) s -- ensure monotyped.  Maybe use tycon isntead of type?
           | TypedRegPtr (Type s) s
           | RegPtrOff s Int
           | VInt Integer
           | VBool Bool
           | Label s
           | VDConsNameTyped (Type s) s
           | VDConsTyped (Type s) s Int [Val s] -- Int is tag/constructor -- val should be registers holding pointers
           | VAddressAt s
           | VHPtr Int
           | VTag Int
               deriving Show


typeOfVal :: Val ByteString -> Type ByteString
typeOfVal v =
    case v of
        VInt {}             -> typeInt
        VDConsNameTyped t _ -> t
        _                   -> error $ show v
