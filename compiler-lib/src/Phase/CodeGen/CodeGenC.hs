module Phase.CodeGen.CodeGenC where

import Common.EitherT          (EitherT (..))
import Common.ReaderT          (ReaderT (..))
import Common.State
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AClosEnv (..), AExp (..), CExp (..), NExp (..), PExp (..), PPat (..), typeOf, typeOfAExp)
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.TypesC

import           Control.Monad               (forM, mapAndUnzipM)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor                ((<&>))
import           Data.Map                    (Map)
import qualified Data.Map as M

type Cg a =
    EitherT ByteString (
        ReaderT [DataDefn ByteString] (
            State (Gen ByteString))) a

data Gen s =
    Gen { nextNum      :: !Int
        , varRegisters :: !(Map s R) 
        }

renderCodeGenC :: [CInstr ByteString] -> ByteString
renderCodeGenC = C8.unlines . map (C8.pack . render)
    where
    render c@CLabel{} = show c
    render c            = "  " <> show c

codeGenModuleC :: AnfModule ByteString
               -> Either ByteString [[CInstr ByteString]]
codeGenModuleC modu = flip evalState initState
                    . flip runReaderT initEnv
                    . runEitherT $ mapM codeGenFunDefn (getFunDefAnfTs modu)

    where
    initState = Gen 0 mempty
    initEnv   = getDataDefnAnfTs modu

codeGenFunDefn :: FunDefAnfT ByteString
               -> Cg [CInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) =
    codeGenNexp nexp <&> \(r, nexp') ->
        concat [ [CLabel name]
               , nexp'
               , [CRet r] ]

codeGenNexp :: NExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenNexp (AExp aexp) = codeGenAexp aexp
codeGenNexp (CExp cexp) = codeGenCexp cexp
codeGenNexp (NLet a b c) = codeGenNLet a b c

codeGenAexp :: AExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenAexp (ATerm t v)           = codeGenATerm t v
codeGenAexp (ALam t vs body)      = codeGenALam t vs body
codeGenAexp (AClo t fvs vs body)  = codeGenAClo t fvs vs body
codeGenAexp (ABinPrimOp t op a b) = codeGenBinPrimOp t op a b

codeGenCexp :: CExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCexp (CApp t f xs)            = codeGenCApp t f xs
codeGenCexp (CAppClo t f env xs)     = codeGenCAppClo t f env xs
codeGenCexp (CIfThenElse t pr tr fl) = codeGenIfThenElse t pr tr fl

codeGenCApp :: Type ByteString -> AExp ByteString -> [AExp ByteString] -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCApp _ f xs = do

    ( f',  fInstrs) <-              codeGenAexp f
    (xs', xsInstrs) <- mapAndUnzipM codeGenAexp xs

    let pushes = reverse $ map CPush xs'

    let envPush = CPush (CLitInt 0) -- Hack

    let call = case f' of
                   CLbl l ->
                       CallLabel l
                   CReg r -> -- Assume closure?
                       CallReg r
    ret <- freshReg
    pure (CReg ret, concat [ fInstrs
                           , concat xsInstrs
                           , pushes
                           , [envPush]
                           , [CCall call]
                           , [CPop ret] ])

codeGenCAppClo :: Type ByteString -> AExp ByteString -> AClosEnv ByteString -> [AExp ByteString] -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCAppClo _ f (AClosEnv env) xs@[] = do

    (f', fInstrs) <- codeGenAexp f

    -- Allocate a closure
    ra <- freshReg
    let sz = 8 + (8 * length env) -- TODO more precise sizing
    let alloc = CAlloc ra sz

    -- Find the registers that need to be copied into the closure
    ers <- forM env $ \er -> do
               mr <- getRegister er
               case mr of
                   Nothing -> error "no such reg"
                   Just r  -> pure $ CReg r

    let closureVals = f' : ers
        closureMovs = zipWith (\o v -> CMov (ToOffsetFrom ra o v)) offsets closureVals

    pure (CReg ra, concat [ fInstrs
                          , [alloc]
                          , closureMovs ])

codeGenNLet :: ByteString -> NExp ByteString -> NExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenNLet a b c = do

    (rb, bs) <- codeGenNexp b

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    ra <- bindFreshReg a -- currently disallows recursion
    let mov = CMov $ case rb of
                         CReg b' -> ToFrom ra b'

    (rc, cs) <- codeGenNexp c

    -- Restore lexical scope
    restoreRegisterMap regMap

    pure (rc, concat [  bs
                     , [mov]
                     ,  cs ])

codeGenIfThenElse _ pr tr fl = do

    fresh        <- freshReg
    branchLables <- mapM freshBranchLabel ["if_", "then_", "else_", "endif_"]
    let [if_, then_, else_, endif_] = branchLables

    (prReg, prInstrs) <- codeGenAexp pr
    (trReg, trInstrs) <- codeGenNexp tr
    (flReg, flInstrs) <- codeGenNexp fl

    (prr, is) <- case prReg of
                     CReg prr -> pure (prr, [])
                     CLitInt i -> do
                        prr <- freshReg
                        pure (prr, [CMov $ FromLitInt prr i])

    let trueMov = case trReg of
                      CReg r    -> ToFrom fresh r
                      CLitInt i -> FromLitInt fresh i

    let falseMov = case flReg of
                       CReg r    -> ToFrom fresh r
                       CLitInt i -> FromLitInt fresh i

    let instrs = concat [ [CLabel if_]
                        ,  prInstrs
                        ,  is
                        , [CCmpB prr]
                        , [Jne else_]

                        , [CLabel then_]
                        ,  trInstrs
                        , [CMov trueMov]
                        , [J endif_]

                        , [CLabel else_]
                        ,  flInstrs
                        , [CMov falseMov]
                        , [CLabel endif_] ]

    pure (CReg fresh, instrs)

codeGenATerm :: Type ByteString -> Term ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenATerm _ (LitInt i)  = pure (CLitInt (fromIntegral i), []) -- TODO: tame fromintegral
codeGenATerm _ (LitBool b) = pure (CLitInt $ if b then 1 else 0, [])
codeGenATerm _ (Var v) = do
    mr <- getRegister v
    case mr of
        Just r ->
            pure (CReg r, [])
        Nothing -> -- Assuming lbl
            pure (CLbl v, [])    

codeGenBinPrimOp :: Type ByteString -> BinOp -> AExp ByteString -> AExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenBinPrimOp _ op a b = do

    (ra, as) <- codeGenAexp a
    (rb, bs) <- codeGenAexp b

    dest <- freshReg

    let instr = case op of
                    EqA  -> [CEq    dest ra rb]
                    AddI -> [CPlus  dest ra rb]
                    SubI -> [CMinus dest ra rb]
                    MulI -> [CTimes dest ra rb]
                    LtI  -> [CLt    dest ra rb]
                    AndB -> [CAnd   dest ra rb]

    pure (CReg dest, concat [ as
                            , bs
                            , instr ])

codeGenAClo :: Type ByteString -> [ByteString] -> [ByteString] -> NExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenAClo _ fvs vs body = do

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    -- Pop the env
    env <- freshReg
    let popEnv = CPop env

    -- Load the env
    fvRegs <- mapM bindFreshReg fvs
    let loads = zipWith (\fv o -> CMov $ ToFromOffset fv env o) fvRegs (tail offsets)

    -- Formal params
    vRegs <- mapM bindFreshReg vs
    let pops = map CPop vRegs

    -- Body
    (rBody, body') <- codeGenNexp body

    -- Restore lexical scope
    restoreRegisterMap regMap

    pure (rBody, concat [ [popEnv]
                        ,  loads
                        ,  pops
                        ,  body' ])

-- TODO: combine with closure code
codeGenALam :: Type ByteString -> [ByteString] -> NExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenALam _ vs body = do

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    -- Pop the env (workaround)
    popEnv <- CPop <$> freshReg

    -- Formal params
    vRegs <- mapM bindFreshReg vs
    let pops = map CPop vRegs

    -- Body
    (rBody, body') <- codeGenNexp body

    -- Restore lexical scope
    restoreRegisterMap regMap

    pure (rBody, concat [ [popEnv]
                        , pops
                        , body' ])

offsets :: [Int]
offsets = map (*8) [0..]

getRegister :: ByteString -> Cg (Maybe R)
getRegister v = lift $ do
    vr <- varRegisters <$> lift get
    pure $ M.lookup v vr

saveRegisterMap :: Cg (Map ByteString R)
saveRegisterMap = lift $ lift (varRegisters <$> get)

restoreRegisterMap :: Map ByteString R -> Cg ()
restoreRegisterMap regMap = lift . lift . modify' $ \gen -> gen { varRegisters = regMap }

bindFreshReg :: ByteString -> Cg R
bindFreshReg v = do
    fresh <- freshReg
    register v fresh
    pure fresh

register :: ByteString -> R -> Cg ()
register var reg = lift . lift . modify' $ \gen -> gen { varRegisters = M.insert var reg (varRegisters gen) }

freshNum :: Cg Int
freshNum = lift . lift $ do
    gen <- get
    let rc = nextNum gen
    put gen { nextNum = rc + 1 }
    pure rc

freshReg :: Cg R
freshReg = R <$> freshNum

freshBranchLabel :: ByteString -> Cg ByteString
freshBranchLabel pre = freshNum <&> \n -> pre <> C8.pack (show n)