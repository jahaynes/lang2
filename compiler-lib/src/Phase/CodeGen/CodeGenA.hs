{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGenA where

import Core.Term               (Term (..))
import Phase.Anf.AnfExpression (AExp (..), NExp (..))
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8

             -- Simple
data AInstr s = ANoOp
              | ALabel s
              | AComment s

             -- Compound Data
              | Allocate Allocable

             -- Control Flow
              | Ret Returnable

                  deriving Show

data Allocable =
    Allocable {- Perhaps String, DataCons, Closure -}
        deriving Show

data Returnable = VirtRegPrim !Int
                | VirtRegPtr !Int
                | RLitInt !Integer
                    deriving Show

renderCodeGenA :: [AInstr ByteString]
               -> ByteString
renderCodeGenA = C8.tail . C8.unlines . map go
    where
    go ANoOp      = "  noop"
    go (ALabel s) = "\n" <> s <> ":"
    go (Ret r)    = "  ret " <> go' r
    go x          = "  renderCodeGenA.go: " <> C8.pack (show x)

    go' (VirtRegPrim n) = "vr_"  <> C8.pack (show n)
    go' (VirtRegPtr n)  = "vrp_" <> C8.pack (show n)
    go' (RLitInt i)     = C8.pack (show i)

codeGenModuleA :: AnfModule ByteString
               -> Either ByteString [AInstr ByteString]
codeGenModuleA modu = concat <$> mapM codeGenFunDefn (getFunDefAnfTs modu)

codeGenFunDefn :: FunDefAnfT ByteString
               -> Either ByteString [AInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) = do
    (r, nexp') <- codeGenNexp nexp
    Right $ concat [ [ALabel name]
                   , nexp'
                   , [Ret r] ]

codeGenNexp :: NExp ByteString
            -> Either ByteString (Returnable, [AInstr ByteString])
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (NLet _ _ _) = Right (unkn, [AComment "nlet"])
codeGenNexp nexp         = Left (C8.pack $ show nexp)

codeGenAexp :: AExp ByteString
            -> Either ByteString (Returnable, [AInstr ByteString])
codeGenAexp (ATerm _type term)          = codeGenTerm term
codeGenAexp (ALam _type _vs _nexp)      = Right (unkn, [AComment "lam"])
codeGenAexp (AClo _type _fvs _vs _nexp) = Right (unkn, [AComment "closure"])
codeGenAexp aexp                        = Left (C8.pack $ show aexp)

codeGenTerm :: Term ByteString
            -> Either ByteString (Returnable, [AInstr ByteString])
codeGenTerm (LitInt i) = Right (RLitInt i, [])
codeGenTerm term       = Left $ "codeGenTerm: " <> C8.pack (show term)

unkn :: Returnable
unkn = VirtRegPtr (-1)
