{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Phase.CodeGen.CodeGenD where

import Common.EitherT          (EitherT (..), left)
import Common.ReaderT          (ReaderT (..))
import Common.State
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AExp (..), CExp (..), NExp (..))
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.TypesD

import           Control.Monad               (forM, mapAndUnzipM)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor                ((<&>))
import           Data.Map                    (Map)
import qualified Data.Map as M
import           Data.List                   (sort)
import           Debug.Trace                 (trace)

type Cg a =
    EitherT ByteString (
        ReaderT [DataDefn ByteString] (
            State (Gen ByteString))) a

data Gen s =
    Gen { nextNum      :: !Int
        , varRegisters :: !(Map s R) 
        }

renderCodeGenD :: [DInstr ByteString] -> ByteString
renderCodeGenD = C8.unlines . map (C8.pack . render)
    where
    render c@DLabel{} = show c
    render c          = "  " <> show c

codeGenModuleD :: AnfModule ByteString
               -> Either ByteString [[DInstr ByteString]]
codeGenModuleD modu = flip evalState initState
                    . flip runReaderT initEnv
                    . runEitherT $ mapM codeGenFunDefn (getFunDefAnfTs modu)

    where
    initState = Gen 0 mempty
    initEnv   = getDataDefnAnfTs modu

codeGenFunDefn :: FunDefAnfT ByteString
               -> Cg [DInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) =
    codeGenNexp nexp <&> \(r, nexp') ->
        concat [ [DLabel name]
               , nexp'
               , [DRet r] ]

codeGenNexp :: NExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (CExp cexp)  = codeGenCexp cexp
codeGenNexp (NLet a b c) = codeGenNLet a b c

codeGenAexp :: AExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenAexp (ATerm t v)           = codeGenATerm t v
codeGenAexp (ALam t vs body)      = codeGenALam t vs body
codeGenAexp (AClo _t _fvs _vs _body) = error "Closures unhandled"
codeGenAexp (AUnPrimOp t op a)    = codeGenUnPrimOp t op a
codeGenAexp (ABinPrimOp t op a b) = codeGenBinPrimOp t op a b

codeGenCexp :: CExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenCexp (CApp t f xs)            = codeGenCApp t f xs
codeGenCexp (CAppClo _t _f _env _xs) = error "Closures unhandled"
codeGenCexp (CIfThenElse t pr tr fl) = codeGenIfThenElse t pr tr fl
codeGenCexp (CCase _t _scrut _ps)    = error "Case unhandled"

codeGenCApp :: Type ByteString -> AExp ByteString -> [AExp ByteString] -> Cg (DVal ByteString, [DInstr ByteString])
codeGenCApp _ f xs = do

    ret <- freshReg

    ( f',  fInstrs) <-              codeGenAexp f
    (xs', xsInstrs) <- mapAndUnzipM codeGenAexp xs

    let f'' = case f' of
                  DLitInt{} -> error "Type error"
                  DReg r    -> CallReg r
                  DLbl l    -> CallLabel l

    pure (DReg ret, concat [ fInstrs
                           , concat xsInstrs
                           , map DPush (reverse xs')
                           , [DCall f'']
                           , [DPop ret]
                           ])

codeGenALam :: Type ByteString -> [ByteString] -> NExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenALam _ fvs body = do

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    pops <- forM fvs $ \fv ->
                DPop <$> bindFreshReg fv

    (rBody, body') <- codeGenNexp body

    -- Restore lexical scope
    restoreRegisterMap regMap    

    pure (rBody, concat [ pops
                        , body'
                        ])

codeGenNLet :: ByteString -> NExp ByteString -> NExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenNLet a b c = do

    (rb, bs) <- codeGenNexp b

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    ra <- bindFreshReg a -- currently disallows recursion
    let mov = DMov $ case rb of
                         DReg b'   -> ToFrom ra b'
                         DLitInt i -> FromLitInt ra i
                         DLbl{}    -> error "TODO DLabel"

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
                  -> Cg (DVal ByteString, [DInstr ByteString])
codeGenIfThenElse _ pr tr fl = do

    fresh                       <- freshReg
    (if_, then_, else_, endif_) <- genBranchLabels

    (prReg, prInstrs) <- codeGenAexp pr
    (trReg, trInstrs) <- codeGenNexp tr
    (flReg, flInstrs) <- codeGenNexp fl

    (prr, is) <- case prReg of
                     DReg prr -> pure (prr, [])
                     DLitInt i -> do
                        prr <- freshReg
                        pure (prr, [DMov $ FromLitInt prr i])
                     DLbl{} -> error "TODO DLbl"

    let trueMov = case trReg of
                      DReg r    -> ToFrom fresh r
                      DLitInt i -> FromLitInt fresh i
                      DLbl{}    -> error "TODO DLbl"

    let falseMov = case flReg of
                       DReg r    -> ToFrom fresh r
                       DLitInt i -> FromLitInt fresh i
                       DLbl{}    -> error "TODO DLbl"

    let instrs = concat [ [DLabel if_]
                        ,  prInstrs
                        ,  is
                        , [DCmpB prr]
                        , [JF else_]

                        , [DLabel then_]
                        ,  trInstrs
                        , [DMov trueMov]
                        , [J endif_]

                        , [DLabel else_]
                        ,  flInstrs
                        , [DMov falseMov]
                        , [DLabel endif_] ]

    pure (DReg fresh, instrs)

    where
    genBranchLabels =
        (,,,) <$> freshBranchLabel "if_"
              <*> freshBranchLabel "then_"
              <*> freshBranchLabel "else_"
              <*> freshBranchLabel "endif_"

codeGenATerm :: Type ByteString -> Term ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenATerm _ (LitInt i)  = pure (DLitInt (fromIntegral i), []) -- TODO: tame fromintegral
codeGenATerm _ (LitBool b) = pure (DLitInt $ if b then 1 else 0, [])
codeGenATerm t (Var v) = mode2
    
    where
    mode2 = getRegister v <&> \case
        Just r  -> (DReg r, [])
        Nothing -> (DLbl v, []) -- Assuming lbl
    -- Old way was better?
{-
    mode1 = getRegister v >>= \case
        Just r  -> pure (DReg r, [])
        Nothing -> do

            -- TODO double-check it's actually a label.
            -- Any constant folding attempt could start by looking here

            ret <- freshReg

            let instrs = [ DCall (CallLabel v)
                         , DPop ret ]

            trace (show t ++ "\n" ++ show v ++ "\n") $ pure (DReg ret, instrs)
-}

codeGenATerm _ LitString{} = error "TODO LitString"
codeGenATerm _ _ = error "codeGenATerm"

codeGenUnPrimOp :: Type ByteString -> UnOp -> AExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenUnPrimOp _ op a = do

    (ra, as) <- codeGenAexp a

    dest <- freshReg

    let mov = case ra of
                  DLitInt{} -> error "DLitInt TODO"
                  DReg r    -> ToFrom dest r
                  DLbl{}    -> error "DLabel TODO"

    instr <- case op of
                 Negate -> pure [ DMov mov
                                , DNeg dest ]
                 EShow  -> left "EShow not impl"
                 Err    -> left "Err not impl"

    pure (DReg dest, concat [ as
                            , instr ])

codeGenBinPrimOp :: Type ByteString -> BinOp -> AExp ByteString -> AExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenBinPrimOp _ op a b = do

    (ra, as) <- codeGenAexp a
    (rb, bs) <- codeGenAexp b

    dest <- freshReg

    let op' = case op of
                    AddI    -> DPlus
                    SubI    -> DMinus
                    MulI    -> DTimes
                    DivI    -> DDiv
                    ModI    -> DMod
                    EqA     -> DEq
                    LtEqI   -> error "TODO LtEqI"
                    LtI     -> DLt
                    GtEqI   -> error "TODO GtEqI"
                    GtI     -> error "TODO GtI"
                    AndB    -> DAnd
                    OrB     -> DOr
                    ConcatS -> error "TODO ConcatS"

    pure (DReg dest, concat [ as
                            , bs
                            , [DBin dest op' ra rb] ])


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

-- ---------------------------------------------------------------------------
-- Virtual Register Lifetime Analysis
-- ---------------------------------------------------------------------------

-- | A live interval tracks the instruction range during which a virtual
--   register holds a value that may be read.  Multiple intervals for the
--   same register occur when the register is reused (re-defined).
data LiveInterval =
    LiveInterval { liRegister :: !R     -- ^ the virtual register
                 , liStart    :: !Int   -- ^ instruction index of definition
                 , liEnd      :: !Int   -- ^ instruction index of last use
                 }
    deriving (Show, Eq, Ord)

-- | Extract the set of registers referenced inside a 'DVal'.
regsOfDVal :: DVal s -> [R]
regsOfDVal (DReg r) = [r]
regsOfDVal _        = []

-- | Registers defined (written) by a single instruction.
instrDefs :: DInstr s -> [R]
instrDefs (DPop r)                         = [r]
instrDefs (DBin r _ _ _)                   = [r]
instrDefs (DNeg r)                         = [r]
instrDefs (DMov (ToFrom r _))              = [r]
instrDefs (DMov (FromLitInt r _))          = [r]
instrDefs (DMov (ToOffsetFrom r _ _))      = [r]
instrDefs (DMov (ToFromOffset r _ _))      = [r]
instrDefs _                                = []

-- | Registers used (read) by a single instruction.
instrUses :: DInstr s -> [R]
instrUses (DPush dv)                       = regsOfDVal dv
instrUses (DCall (CallReg r))              = [r]
instrUses (DBin _ _ a b)                   = regsOfDVal a ++ regsOfDVal b
instrUses (DNeg r)                         = [r]
instrUses (DCmpB r)                        = [r]
instrUses (DMov (ToFrom _ r))              = [r]
instrUses (DMov (ToOffsetFrom _ _ dv))     = regsOfDVal dv
instrUses (DMov (ToFromOffset _ r _))      = [r]
instrUses (DRet dv)                        = regsOfDVal dv
instrUses (DFun fvs _)                     = concatMap regsOfDVal fvs
instrUses _                                = []

-- | Compute live intervals for all virtual registers in an instruction
--   list.  Returns a pair:
--
--   (1) intervals for the top-level instruction list, sorted by start
--       position then by register;
--   (2) a list of interval-sets, one per 'DFun' encountered, in
--       left-to-right order.  Nested functions are analysed recursively.
computeLifetimes :: [DInstr s] -> ([LiveInterval], [[LiveInterval]])
computeLifetimes instrs =
    run 0 instrs M.empty M.empty [] []
  where

    -- run :: idx -> remaining -> open -> lastUse -> closed -> nested -> result
    run _ [] open lastUse closed nested =
        let finalised = finalise open lastUse
        in (sort (closed ++ finalised), reverse nested)

    run i (inst : rest) open lastUse closed nested =
        let -- 1. Record uses (extend lifetimes of live-in / open intervals)
            --    IMPORTANT: uses are processed BEFORE defs. This ensures that
            --    instructions that both read and write the same register (e.g.
            --    'DNeg r') correctly close the old interval at position i
            --    (including the read) and start a new interval at i (after the
            --    write).
            open'  = foldl' (recordUse i) open  (instrUses inst)
            lu'    = foldl' (recordLastUse i) lastUse (instrUses inst)

            -- 2. Record defs (close old intervals, start new ones)
            (open'', lu'', closed') =
                foldl' (applyDef i) (open', lu', closed) (instrDefs inst)

            -- 3. Recurse into DFun bodies
            nested' = case inst of
                          DFun _ body ->
                              let (inner, _innerNest) = computeLifetimes body
                              in inner : nested
                          _ -> nested
        in run (i + 1) rest open'' lu'' closed' nested'

    -- | If @r@ has an open interval, extend its last-use to @i@.
    --   If @r@ is used with *no* open interval, treat it as live-in:
    --   start a pseudo-interval at 0.
    recordUse :: Int -> Map R Int -> R -> Map R Int
    recordUse _ open r =
        case M.lookup r open of
            Just _  -> open        -- already tracked, lastUse handled separately
            Nothing -> M.insert r 0 open   -- live-in: start at 0

    -- | Record the last use position for a register.
    recordLastUse :: Int -> Map R Int -> R -> Map R Int
    recordLastUse i lu r = M.insert r i lu

    -- | Process a definition of @r@ at index @i@:
    --   close any existing open interval and start a new one.
    applyDef :: Int
             -> (Map R Int, Map R Int, [LiveInterval])
             -> R
             -> (Map R Int, Map R Int, [LiveInterval])
    applyDef i (open, lu, closed) r =
        let -- Close previous interval if it exists
            closed' = case M.lookup r open of
                          Nothing -> closed
                          Just s  ->
                              let e = M.findWithDefault s r lu
                              in LiveInterval r s e : closed
            -- Start new interval at i
            open'  = M.insert r i open
            -- Clear last-use for the new interval
            lu'    = M.delete r lu
        in (open', lu', closed')

    -- | Close any intervals still open at the end of the instruction list.
    finalise :: Map R Int -> Map R Int -> [LiveInterval]
    finalise open lu =
        [ LiveInterval r s (M.findWithDefault s r lu)
        | (r, s) <- M.toList open ]