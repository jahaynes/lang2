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

    pure (DReg ret, concat [ [DComment "Begin codeGenCApp"]
                           , fInstrs
                           , concat xsInstrs
                           , map DPush (reverse xs')
                           , [DCall f'']
                           , [DPop ret]
                           , [DComment "End codeGenCApp"]
                           ])

codeGenALam :: Type ByteString -> [ByteString] -> NExp ByteString -> Cg (DVal ByteString, [DInstr ByteString])
codeGenALam _ fvs body = do

    ret <- freshReg

    -- Preserve lexical scope
    regMap <- saveRegisterMap

    -- pop some fvs, forward order
    pops <- forM fvs $ \fv -> do
                r <- bindFreshReg fv
                pure $ DPop r

    -- process body
    (rb, bs) <- codeGenNexp body

    -- Opportunity to return register directly here
    let after = case rb of
                    DLitInt{} -> error "TODO DLitInt"
                    DReg r    -> DMov (ToFrom ret r)
                    DLbl{}    -> error "TODO DLbl"

    -- Restore lexical scope
    restoreRegisterMap regMap    

    pure (DReg ret, concat [ [DComment "Begin codeGenALam"]
                           , pops
                           , bs
                           , [after]
                           , [DComment "End codeGenALam"]
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
                        , [Jne else_]

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
codeGenATerm _ (Var v) =
    getRegister v <&> \case
        Just r  -> (DReg r, [])
        Nothing -> (DLbl v, []) -- Assuming lbl
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

    -- TODO this can be smoothed out
    let instr = case op of
                    AddI    -> [DBin dest DPlus  ra rb]
                    SubI    -> [DBin dest DMinus ra rb]
                    MulI    -> [DBin dest DTimes ra rb]
                    DivI    -> [DBin dest DDiv   ra rb]
                    ModI    -> [DBin dest DMod   ra rb]
                    EqA     -> [DBin dest DEq    ra rb]
                    LtEqI   -> error "TODO LtEqI"
                    LtI     -> [DBin dest DLt    ra rb]
                    GtEqI   -> error "TODO GtEqI"
                    GtI     -> error "TODO GtI"
                    AndB    -> [DBin dest DAnd   ra rb]
                    OrB     -> [DBin dest DOr    ra rb]
                    ConcatS -> error "TODO ConcatS"

    pure (DReg dest, concat [ as
                            , bs
                            , instr ])

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