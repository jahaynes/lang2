module Phase.CodeGen.Val where

import Core.Types

data Val s = Reg s
           | TypedReg (Type s) s -- ensure monotyped.  Maybe use tycon isntead of type?
           | TypedRegPtr (Type s) s
           | RegPtrOff s Int
           | VInt Integer
           | VBool Bool
           | Label s
           | VDConsName s
           | VDCons s Int [Val s] -- Int is tag/constructor -- val should be registers holding pointers
           | VAddressAt s
           | VHPtr Int
           | VTag Int
               deriving Show