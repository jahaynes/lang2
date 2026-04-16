{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Phase.CodeGen.CodeGenC where

import Common.EitherT          (EitherT (..), left)
import Common.ReaderT          (ReaderT (..), ask)
import Common.State
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression -- (AClosEnv (..), AExp (..), CExp (..), NExp (..))
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.SizeInfo
import Phase.CodeGen.TagInfo
import Phase.CodeGen.TypesC
import TypeSystem.Common       (Subst (..))

import           Control.Monad               (forM, mapAndUnzipM)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor                ((<&>))
import           Data.Map                    ((!), Map)
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
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (CExp cexp)  = codeGenCexp cexp
codeGenNexp (NLet a b c) = codeGenNLet a b c

codeGenAexp :: AExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenAexp (ATerm t v)           = codeGenATerm t v
codeGenAexp (ALam t vs body)      = codeGenALam t vs body
codeGenAexp (AClo t fvs vs body)  = codeGenAClo t fvs vs body
codeGenAexp (AUnPrimOp t op a)    = codeGenUnPrimOp t op a
codeGenAexp (ABinPrimOp t op a b) = codeGenBinPrimOp t op a b

codeGenCexp :: CExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCexp (CApp t f xs)            = codeGenCApp t f xs
codeGenCexp (CAppClo t f env xs)     = codeGenCAppClo t f env xs
codeGenCexp (CIfThenElse t pr tr fl) = codeGenIfThenElse t pr tr fl
codeGenCexp (CCase t scrut ps)       = codeGenCase t scrut ps

codeGenCase :: Type ByteString
            -> AExp ByteString
            -> [PExp ByteString]
            -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCase _ scrut ps = do

    lhsTypeMap <- instantiateLhs (typeOfAExp scrut)

    -- Return to this reg
    destReg <- freshReg

    -- Handle the scrutinee
    (scrutReg, scrutInstrs) <- codeGenAexp scrut
    let CReg scrutReg' = scrutReg

    -- Prepare a register to hold the tag
    tag <- freshReg

    -- A register for comparing the tag
    sharedCmp <- freshReg

    -- Prepare a 'done' label for all branches to jump to
    doneLabel <- freshBranchLabel "done_"

    checksAndLoads <- forM ps $ \(PExp lhs rhs) -> do

        -- Get the tag num to check against
        let PApp dc _ _ = lhs
            Just (i, dctypes) = M.lookup dc lhsTypeMap

        -- Make a label for this pattern
        label <- freshBranchLabel ("pat_" <> C8.pack (show i) <> "_" <> dc <> "_")

                          -- write the comparison bool to sharedCmp
        let checkInstrs = [ CEq sharedCmp (CReg tag) (CLitInt i)

                          -- Check the comparison
                          , CCmpB sharedCmp

                          -- Conditional jmp to label
                          , Je label ]

        regMap0  <- saveRegisterMap
        lhsLoads <- codeGenPattern dctypes label scrutReg lhs rhs destReg doneLabel
        restoreRegisterMap regMap0
        pure (checkInstrs, lhsLoads)

    let (checks, loads) = unzip checksAndLoads

    -- Prepare an 'inexhause' label if the scrutinee doesn't match any of the patterns
    inexhaust_ <- freshBranchLabel "inexhaust_"

    let instrs = concat [ scrutInstrs
                        , [ CComment "Checking constructor tag"
                          , CMov $ ToFromOffset tag scrutReg' 0 ]
                        , concat checks
                        , [ J inexhaust_ ]
                        , concat loads
                        , [ CLabel inexhaust_
                          , CErr "inexhaust" ]
                        , [ CLabel doneLabel ]
                        ]

    pure (CReg destReg, instrs)

-- TODO: Assumes Constructor.  No literals yet?
codeGenPattern :: [Type ByteString]
               -> ByteString
               -> CVal ByteString
               -> PPat ByteString
               -> NExp ByteString
               -> R
               -> ByteString
               -> Cg [CInstr ByteString]
codeGenPattern dctypes label (CReg scrutReg) (PApp dc dct ms) rhs destReg doneLabel = do
    -- Get the offsets of each pattern's field
    offsets <- fieldOffsets <$> sizeOfDConsInstance dc dct

    lhsInstrs <- forM (zip3 dctypes ms offsets) $ \case
        (t, Var v, off) -> do
            fresh <- freshReg
            register v fresh
            pure . CMov $ ToFromOffset fresh scrutReg off

    -- Generate code for the RHS
    (rhsReg, rhsInstrs) <- codeGenNexp rhs

    let mov = case rhsReg of
                  CReg r     -> CMov $ ToFrom destReg r
                  CLitInt i  -> CMov $ FromLitInt destReg i
                  _          -> error $ "codeGenPattern.mov: " ++ show rhsReg

    pure  $ CLabel label
          : lhsInstrs
         ++ rhsInstrs
         ++ [ mov
            , J doneLabel ]

-- For each data constructor, return its tag num and the concrete types of its members
instantiateLhs :: Type ByteString -> Cg (Map ByteString (Int, [Type ByteString]))
instantiateLhs (TyCon tc typeInsts) = do

    -- Lookup the right set of data constructors and their type variables
    DataDefn _ tyVars dcons <- head -- TODO error handle
                             . filter (\(DataDefn t _ _) -> t == tc)
                           <$> lift ask

    -- Prepare the variable-to-concrete-type map
    let subst = Subst . M.fromList $ zip tyVars typeInsts

    -- Instantiate all the data constructors
    let dcons' = map (apply subst) dcons

    pure . M.fromList $ zipWith (\(dc, ts) i -> (dc, (i, ts))) dcons' [0..]

    where
    apply :: forall s. (Ord s, Show s) => Subst s -> DataCon s -> (s, [Type s])
    apply (Subst subst) (DataCon n ms) = (n, map go ms)
        where
        go :: Member s -> Type s
        go (MemberType n' tv) = TyCon n' (map go tv)
        go (MemberVar v)      = subst ! v

codeGenCApp :: Type ByteString -> AExp ByteString -> [AExp ByteString] -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCApp _ f xs = do

    ( f',  fInstrs) <-              codeGenAexp f
    (xs', xsInstrs) <- mapAndUnzipM codeGenAexp xs

    let pushes = reverse $ map CPush xs'

    let envPush = CPush (CLitInt 0) -- Hack

    let call = case f' of
                   CLitInt{} ->
                       error "Impossible"
                   CLbl l ->
                       CallLabel l
                   CReg r -> -- Assume closure?
                       CallReg r
    ret <- freshReg
    pure (CReg ret, concat [ fInstrs
                           , concat xsInstrs
                           , pushes
                           , [envPush]
                           , [CCall call (length (envPush:pushes)) 1]
                           , [CPop ret] ])

codeGenCAppClo :: Type ByteString -> AExp ByteString -> AClosEnv ByteString -> [AExp ByteString] -> Cg (CVal ByteString, [CInstr ByteString])
codeGenCAppClo _ _ _ (_:_) = error "TODO?"
codeGenCAppClo _ f (AClosEnv env) [] = do

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
                         CReg b'   -> ToFrom ra b'
                         CLitInt{} -> error "TODO CLitInt"
                         CLbl{}    -> error "TODO CLabel"

    (rc, cs) <- codeGenNexp c

    -- Restore lexical scope
    restoreRegisterMap regMap

    pure (rc, concat [  bs
                     , [mov]
                     ,  cs ])

codeGenIfThenElse :: Type ByteString
                  -> AExp ByteString
                  -> NExp ByteString
                  -> NExp ByteString
                  -> Cg (CVal ByteString, [CInstr ByteString])
codeGenIfThenElse _ pr tr fl = do

    fresh                       <- freshReg
    (if_, then_, else_, endif_) <- genBranchLabels

    (prReg, prInstrs) <- codeGenAexp pr
    (trReg, trInstrs) <- codeGenNexp tr
    (flReg, flInstrs) <- codeGenNexp fl

    (prr, is) <- case prReg of
                     CReg prr -> pure (prr, [])
                     CLitInt i -> do
                        prr <- freshReg
                        pure (prr, [CMov $ FromLitInt prr i])
                     CLbl{} -> error "TODO CLbl"

    let trueMov = case trReg of
                      CReg r    -> ToFrom fresh r
                      CLitInt i -> FromLitInt fresh i
                      CLbl{} -> error "TODO CLbl"

    let falseMov = case flReg of
                       CReg r    -> ToFrom fresh r
                       CLitInt i -> FromLitInt fresh i
                       CLbl{} -> error "TODO CLbl"

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

    where
    genBranchLabels = do
        (,,,) <$> freshBranchLabel "if_"
              <*> freshBranchLabel "then_"
              <*> freshBranchLabel "else_"
              <*> freshBranchLabel "endif_"

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
codeGenATerm _ LitString{} = error "TODO LitString"

-- Just a tag 
codeGenATerm t (DCons dc) = do
    rr    <- freshReg
    Tag n <- getTag t dc
    pure (CReg rr, [ CAlloc rr 8
                   , CMov $ ToOffsetFrom rr 0 (CLitInt n) ])

codeGenUnPrimOp :: Type ByteString -> UnOp -> AExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenUnPrimOp _ op a = do

    (ra, as) <- codeGenAexp a

    dest <- freshReg

    let mov = case ra of
                  CReg r -> ToFrom dest r

    instr <- case op of
                 Negate -> pure [ CMov mov
                                , CNeg dest ]
                 EShow  -> left "EShow not impl"
                 Err    -> left "Err not impl"

    pure (CReg dest, concat [ as
                            , instr ])

codeGenBinPrimOp :: Type ByteString -> BinOp -> AExp ByteString -> AExp ByteString -> Cg (CVal ByteString, [CInstr ByteString])
codeGenBinPrimOp _ op a b = do

    (ra, as) <- codeGenAexp a
    (rb, bs) <- codeGenAexp b

    dest <- freshReg

    let instr = case op of
                    AddI    -> [CPlus  dest ra rb]
                    SubI    -> [CMinus dest ra rb]
                    MulI    -> [CTimes dest ra rb]
                    DivI    -> [CDiv   dest ra rb]
                    ModI    -> [CMod   dest ra rb]
                    EqA     -> [CEq    dest ra rb]
                    LtEqI   -> error "TODO LtEqI"
                    LtI     -> [CLt    dest ra rb]
                    GtEqI   -> error "TODO GtEqI"
                    GtI     -> error "TODO GtI"
                    AndB    -> [CAnd   dest ra rb]
                    OrB     -> [COr    dest ra rb]
                    ConcatS -> error "TODO ConcatS"

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