{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Phase.CodeGen.CodeGenC where

import Common.EitherT          (EitherT (..), left)
import Common.ReaderT          (ReaderT (..), ask)
import Common.State
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AClosEnv (..), AExp (..), CExp (..), NExp (..), PExp (..), PPat (..), typeOf, typeOfAExp)
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.SizeInfo
import Phase.CodeGen.TagInfo
import Phase.CodeGen.TypesC
import TypeSystem.Common       (Subst (..))

import           Control.Monad               (forM, replicateM, zipWithM)
import           Data.ByteString.Char8       (ByteString, pack)
import qualified Data.ByteString.Char8 as C8
import           Data.Char     (isSpace)
import           Data.Functor  ((<&>))
import           Data.Map      ((!), Map)
import qualified Data.Map as M
import           Debug.Trace (trace)

type Cg a =
    EitherT ByteString (
        ReaderT [DataDefn ByteString] (
            State (Gen ByteString))) a

data Gen s =
    Gen { regCount     :: !Int
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
codeGenAexp x                     = error $ show ("codeGenAexp", x)

codeGenCexp (CApp t f xs)         = codeGenCApp t f xs
codeGenCexp (CAppClo t f env xs)  = codeGenCAppClo t f env xs
codeGenCexp x                     = error $ show ("codeGenCexp", x)

codeGenCApp _ f xs = do

    ( f',  fInstrs) <-                codeGenAexp f
    (xs', xsInstrs) <- unzip <$> mapM codeGenAexp xs

    let pushes = reverse $ map CPush xs'

    let call = CCall $ case f' of
                          CLbl l ->
                              CallLabel l
                          CReg r -> -- Assume closure?
                              CallReg r
    ret <- freshReg
    let pop = CPop ret
    pure (CReg ret, concat [ fInstrs
                           , concat xsInstrs
                           , pushes
                           , [call]
                           , [pop] ])

codeGenCAppClo _ f (AClosEnv env) xs = do
    
    ( f',  fInstrs) <-                codeGenAexp f
    (xs', xsInstrs) <- unzip <$> mapM codeGenAexp xs

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

    pure (CReg ra, concat [ [alloc]
                          , closureMovs ])

codeGenNLet a b c = do

    (rb, bs) <- codeGenNexp b

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    ra <- bindFreshReg a -- currently disallows recursion
    let mov = CMov $ case rb of
                         CReg b -> ToFrom ra b

    (rc, cs) <- codeGenNexp c

    -- Restore lexical scope
    restoreRegisterMap regMap

    pure (rc, concat [  bs
                     , [mov]
                     ,  cs ])

codeGenATerm :: Type ByteString -> Term ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenATerm t (LitInt i) = do
    let i' = fromIntegral i -- TODO
    pure (CLitInt i', [])

codeGenATerm t (Var v) = do
    mr <- getRegister v
    case mr of
        Just r ->
            pure (CReg r, [])
        Nothing -> -- Assuming lbl
            pure (CLbl v, [])    

codeGenBinPrimOp _ op a b = do

    (ra, as) <- codeGenAexp a
    (rb, bs) <- codeGenAexp b

    dest <- freshReg

    let instr = case op of
                    AddI -> [CPlus  dest ra rb]
                    MulI -> [CTimes dest ra rb]

    pure (CReg dest, concat [ as
                            , bs
                            , instr ])

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

-- combine with closure code
codeGenALam _ vs body = do

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    -- Formal params
    vRegs <- mapM bindFreshReg vs
    let pops = map CPop vRegs

    -- Body
    (rBody, body') <- codeGenNexp body

    -- Restore lexical scope
    restoreRegisterMap regMap

    pure (rBody, concat [ pops
                        , body' ])


{-
anf0Instr :: State R [CInstr Text]
anf0Instr = do

    -- Env { xx }
    env <- freshR
    let popEnv = CPop env
    xx <- freshR
    let readXx = CMov $ ToFromOffset xx env 8

    -- Format params
    y <- freshR
    let popY = CPop y
    yxx <- freshR

    let plus = CPlus yxx (CReg y) (CReg xx)
    let ret  = CRet (CReg yxx)

    pure [CLabel "anf_0", popEnv, readXx, popY, plus, ret]
-}

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

freshReg :: Cg R
freshReg = lift . lift $ do
    gen <- get
    let rc = regCount gen
    put gen { regCount = rc + 1 }
    pure $ R rc