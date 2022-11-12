module Phase.CodeGen.Val where

data Val s = Reg s
           | RegPtr s
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