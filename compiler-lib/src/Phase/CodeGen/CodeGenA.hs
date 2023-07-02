{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Phase.CodeGen.CodeGenA (AInstr, codeGenModuleA, renderCodeGenA) where

import Common.EitherT (EitherT (..), left)
import Common.State
import Common.Trans
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AExp (..), CExp (..), NExp (..), typeOf, typeOfAExp)
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.TypesA

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor  ((<&>))
import           Data.Map      (Map)
import qualified Data.Map as M

type Cg a =
    EitherT ByteString (State (Gen ByteString)) a

data Gen s =
    Gen { regCount     :: !Int
        , varRegisters :: !(Map s SVal) 
        }

renderCodeGenA :: [AInstr ByteString]
               -> ByteString
renderCodeGenA = C8.tail . C8.unlines . map go
    where
    go (ALabel s)   = "\n" <> s <> ":"
    go (AComment s) = "  // " <> s

    go (AMov dst src) = "  " <> go' dst <> " <- " <> go' src
    go (ABinOp dst op a b) = "  " <> go' dst <> " <- " <> go'' op <> " " <> go' a <> " " <> go' b
    go (ACmp a) = "  cmp " <> go' a

    go (Allocate dst (ADataCons t dc)) = "  " <> go' dst <> " <- alloc " <> go'' t <> "." <> dc

    go (Push dbgName t val) = "  push " <> go' val <> " :: " <> go'' t <> " // '" <> dbgName <> "'"
    go (Pop  dbgName t val) = "  " <> go' val <> " <- pop :: " <> go'' t <> " // '" <> dbgName <> "'" 
    go (Call f)   = "  call " <> f
    go (Ret r)    = "  ret " <> go' r
    go (J lbl)    = "  j " <> lbl
    go (Jne lbl)  = "  jne " <> lbl
    go x          = "  renderCodeGenA.go: " <> C8.pack (show x)

    go' (VirtRegPrim n) = "vr_"  <> C8.pack (show n)
    go' (VirtRegPtr n)  = "vrp_" <> C8.pack (show n)
    go' (RLitBool b)    = C8.pack (show b)
    go' (RLitInt i)     = C8.pack (show i)

    go'' t = C8.pack (show t)

codeGenModuleA :: AnfModule ByteString
               -> Either ByteString [AInstr ByteString]
codeGenModuleA modu = 
    let xs = concat <$> mapM codeGenFunDefn (getFunDefAnfTs modu)
    in evalState (runEitherT xs) (Gen 0 mempty)

codeGenFunDefn :: FunDefAnfT ByteString
               -> Cg [AInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) =
    codeGenNexp nexp <&> \(r, nexp') ->
        concat [ [ALabel name]
               , nexp'
               , [Ret r] ]

codeGenNexp :: NExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (CExp cexp)  = codeGenCexp cexp
codeGenNexp (NLet a b c) = codeGenNlet a b c

codeGenAexp :: AExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenAexp (ATerm _type term)          = codeGenTerm term
codeGenAexp (ALam t vs nexp)            = codegenLam t vs nexp
codeGenAexp (AClo _type _fvs _vs _nexp) = pure (unkn, [AComment "closure"])

codeGenAexp (ABinPrimOp t op a b) = do
    (areg, aInstrs) <- codeGenAexp a
    (breg, bInstrs) <- codeGenAexp b
    dest <- freshRegisterFor t
    let instrs = concat [ aInstrs
                        , bInstrs
                        , [ABinOp dest op areg breg] ]
    pure (dest, instrs)

codeGenAexp aexp = left $ "aexp: " <> C8.pack (show aexp)

codegenLam :: Type ByteString
           -> [ByteString]
           -> NExp ByteString
           -> Cg (SVal, [AInstr ByteString])
codegenLam t vs nexp = do

    regMap0 <- saveRegisterMap

    pops <- popsForwardOrder t vs
    (ret, nexp') <- codeGenNexp nexp
    let instrs = concat [ pops
                        , nexp' ]
    restoreRegisterMap regMap0
    pure (ret, instrs)

codeGenNlet :: ByteString
            -> NExp ByteString
            -> NExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenNlet a b c = do

    fresh <- freshRegisterFor (typeOf b)
    register a fresh

    (breg, bInstrs) <- codeGenNexp b
    (creg, cInstrs) <- codeGenNexp c

    let instrs = concat [ bInstrs
                        , [AMov fresh breg]
                        , cInstrs ]

    pure (creg, instrs)

bind :: Type s -> [a] -> [(Type s, a)]
bind (TyArr a b) (v:vs) = (a,v) : bind b vs
bind           _     [] = []

popsForwardOrder :: Type ByteString
                 -> [ByteString]
                 -> Cg [AInstr ByteString]
popsForwardOrder ty = mapM go . bind ty
    where
    go (t, v) = do
        fresh <- freshRegisterFor t
        register v fresh
        pure $ Pop v t fresh

-- todo fold
pushesReverseOrder :: [(AExp ByteString, SVal)] -> [AInstr ByteString]
pushesReverseOrder = go []
    where
    go acc               [] = acc
    go acc ((aexp, val):xs) = go (Push (describe aexp) (typeOfAExp aexp) val:acc) xs
        where
        describe (ABinPrimOp _ AddI _ _) = "+"
        describe (ABinPrimOp _ SubI _ _) = "-"
        describe (ATerm _ (Var v)) = v
        describe (ATerm _ (LitInt i)) = C8.pack (show i)
        describe x = "Not described: " <> C8.pack (show x)

codeGenCexp :: CExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenCexp (CApp t f xs) = codeGenApp t f xs
codeGenCexp (CIfThenElse t pr tr fl) = codeGenIfThenElse t pr tr fl
codeGenCexp cexp = left $ "codeGenCexp: " <> C8.pack (show cexp)

codeGenApp :: Type ByteString
           -> AExp ByteString
           -> [AExp ByteString]
           -> Cg (SVal, [AInstr ByteString])

codeGenApp t (ATerm _ (DCons dc)) xs = do

    (_, _) <- unzip <$> mapM codeGenAexp xs

    rr <- freshRegisterFor t

    -- do more
    let instrs = [Allocate rr (ADataCons t dc)]

    pure (unkn, instrs)

codeGenApp t (ATerm _ (Var v)) xs = do

    -- Get the args ready to be pushed
    (xs', prePushInstrs) <- unzip <$> mapM codeGenAexp xs
    let pushes = pushesReverseOrder (zip xs xs')

    rr <- freshRegisterFor t

    let instrs = concat [ concat prePushInstrs
                        , pushes
                        , [ Call v
                          , Pop ("ret from " <> v) t rr ] ]

    pure (rr, instrs)

codeGenIfThenElse t pr tr fl = do
    rr                          <- freshRegisterFor t
    (if_, then_, else_, endif_) <- freshBranchLabels
    (prReg, prInstrs)           <- codeGenAexp pr
    (trReg, trInstrs)           <- codeGenNexp tr
    (flReg, flInstrs)           <- codeGenNexp fl

    let instrs = concat [ [ALabel if_]
                        , prInstrs
                        , [ ACmp prReg
                          , Jne else_ ]
                        , [ALabel then_]
                        , trInstrs
                        , [ AMov rr trReg
                          , J endif_ ]
                        , [ALabel else_]
                        , flInstrs
                        , [ AMov rr flReg
                          , ALabel endif_] ]

    pure (rr, instrs)

codeGenTerm :: Term ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenTerm (LitBool b) = pure (RLitBool b, [])
codeGenTerm (LitInt i)  = pure (RLitInt i, [])
codeGenTerm (Var v) =
    getRegister v >>= \case
        Just r  -> pure (r, [])
        Nothing -> left $ "Unregistered: " <> v -- pure (unkn, []).  Unapplied functions can get snagged here
codeGenTerm term = left $ "codeGenTerm: " <> C8.pack (show term)

getRegister :: ByteString -> Cg (Maybe SVal)
getRegister v = lift $ do
    vr <- varRegisters <$> get
    pure $ M.lookup v vr

freshRegisterFor :: Type ByteString -> Cg SVal
freshRegisterFor t =
    freshNum <&> \rc ->
        case t of
            TyCon "Int"  [] -> VirtRegPrim rc
            TyCon "Bool" [] -> VirtRegPrim rc
            _               -> VirtRegPtr rc

freshBranchLabels :: Cg (ByteString, ByteString, ByteString, ByteString)
freshBranchLabels =
    freshNum <&> \n ->
        let n' = C8.pack (show n)
        in ( "if_" <> n'
           , "then_" <> n'
           , "else_" <> n'
           , "endif_" <> n' )

freshNum :: Cg Int
freshNum = do
    gen <- lift get
    let rc = regCount gen
    lift $ put gen { regCount = rc + 1 }
    pure rc

unkn :: SVal
unkn = VirtRegPtr 99

register :: ByteString -> SVal -> Cg ()
register var reg = lift . modify' $ \gen -> gen { varRegisters = M.insert var reg (varRegisters gen) }

saveRegisterMap :: Cg (Map ByteString SVal)
saveRegisterMap = lift (varRegisters <$> get)

restoreRegisterMap :: Map ByteString SVal -> Cg ()
restoreRegisterMap regMap = lift . modify' $ \gen -> gen { varRegisters = regMap }