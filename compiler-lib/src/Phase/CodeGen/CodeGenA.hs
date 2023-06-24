{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGenA where

import Common.EitherT (EitherT (..), left)
import Common.State
import Common.StateT
import Common.Trans
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AExp (..), CExp (..), NExp (..))
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8

type Cg a =
    EitherT ByteString (State Gen) a

newtype Gen =
    Gen { regCount :: Int }

             -- Simple
data AInstr s = ANoOp
              | ALabel s
              | AComment s

             -- Compound Data
              | Allocate Allocable

             -- Control Flow
              | Push s (Type s) SVal -- debugname / type / idunno?
              | Pop  s (Type s) SVal -- debugname / type / val
              | Call s
              | Ret SVal

                  deriving Show

data Allocable =
    Allocable {- Perhaps String, DataCons, Closure -}
        deriving Show

data SVal = VirtRegPrim !Int
          | VirtRegPtr !Int
          | RLitInt !Integer
             deriving Show

renderCodeGenA :: [AInstr ByteString]
               -> ByteString
renderCodeGenA = C8.tail . C8.unlines . map go
    where
    go ANoOp        = "  noop"
    go (ALabel s)   = "\n" <> s <> ":"
    go (AComment s) = "  // " <> s

    go (Push dbgName t val) = "  push " <> go' val <> " :: " <> go'' t <> " // '" <> dbgName <> "'"
    go (Pop  dbgName t val) = "  " <> go' val <> " <- pop :: " <> go'' t <> " // '" <> dbgName <> "'" 
    go (Call f)   = "  call " <> f
    go (Ret r)    = "  ret " <> go' r
    go x          = "  renderCodeGenA.go: " <> C8.pack (show x)

    go' (VirtRegPrim n) = "vr_"  <> C8.pack (show n)
    go' (VirtRegPtr n)  = "vrp_" <> C8.pack (show n)
    go' (RLitInt i)     = C8.pack (show i)

    go'' t = C8.pack (show t)

codeGenModuleA :: AnfModule ByteString
               -> Either ByteString [AInstr ByteString]
codeGenModuleA modu = 
    let xs = concat <$> mapM codeGenFunDefn (getFunDefAnfTs modu)
    in evalState (runEitherT xs) (Gen 0)

codeGenFunDefn :: FunDefAnfT ByteString
               -> Cg [AInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) = do
    (r, nexp') <- codeGenNexp nexp
    pure $ concat [ [ALabel name]
                  , nexp'
                  , [Ret r] ]

-- perhaps aexp and cexp can vary in how they accept/return binders?

codeGenNexp :: NExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (CExp cexp)  = codeGenCexp cexp
codeGenNexp (NLet _ _ _) = pure (unkn, [AComment "nlet"])
codeGenNexp nexp         = left (C8.pack $ show nexp)

codeGenAexp :: AExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenAexp (ATerm _type term)          = codeGenTerm term
codeGenAexp (ALam t vs nexp)            = codegenLam t vs nexp
codeGenAexp (AClo _type _fvs _vs _nexp) = pure (unkn, [AComment "closure"])
codeGenAexp aexp                        = left (C8.pack $ show aexp)

codegenLam t vs nexp = do

    pops <- expressPop t vs

    (ret, nexp') <- codeGenNexp nexp

    let instrs = concat [ pops
                        , nexp' ]

    pure (ret, instrs)


-- TODO think this one out:
bind (TyArr a b) (v:vs) = (a,v) : bind b vs
bind t               [] = []

-- forward order
expressPop t vs =
    mapM go (bind t vs)
    where
    go (t, v) = Pop v t <$> freshRegisterFor t

-- reverse order
expressPushes =
    mapM go . reverse
    where
    go (ATerm t (LitInt i)) = pure $ Push "lit" t (RLitInt i)

codeGenCexp :: CExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenCexp (CApp t f xs) = codeGenApp t f xs

codeGenApp t (ATerm _ (Var v)) xs = do
    pushes <- expressPushes xs
    let instrs = concat [ [AComment "app"]
                        , pushes
                        , [Call v]
                        ]
    pure (unkn, instrs)


codeGenTerm :: Term ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenTerm (LitInt i) = pure (RLitInt i, [])
codeGenTerm (Var v)    = error "lookup bound register"
codeGenTerm term       = left $ "codeGenTerm: " <> C8.pack (show term)

unkn :: SVal
unkn = VirtRegPtr 99

freshRegisterFor :: Type ByteString -> Cg SVal
freshRegisterFor t = do
    gen <- lift get
    let rc = regCount gen
    lift $ put gen { regCount = rc + 1 }
    pure $ case t of
        TyCon "Int"  [] -> VirtRegPrim rc
        TyCon "Bool" [] -> VirtRegPrim rc
        _               -> VirtRegPtr rc