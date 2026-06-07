{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             QuasiQuotes,
             ScopedTypeVariables #-}

module Phase.CodeGen.CodeGenD ( codeGenModuleD
                              , renderCodeGenD
                              ) where

-- import Common.CallGraph
import Common.EitherT          (EitherT (..), left)
import Common.ReaderT          (ReaderT (..), ask, runReaderT')
-- import Common.State
import Common.StateT           (StateT, evalStateT', get, modify, put)
import Common.Trans            (lift)
--import Core.Module
import Core.Operator
import Core.Term               (Term (..))
--import Core.Types
import Phase.Anf.AnfExpression (AExp (..), CExp (..), NExp (..), typeOf)
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.TypesD

import           Control.Monad               (forM, unless)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
-- import           Data.Functor                ((<&>))
import           Data.Functor.Identity
import           Data.Map                    (Map)
import qualified Data.Map.Strict as M
import           Data.Set                    (Set)
import qualified Data.Set as S
import           Data.String.Interpolate (i)
-- import           Debug.Trace                 (trace)

err :: Monad m => ByteString -> CgM m a
err msg = left msg


renderCodeGenD :: [DInstr ByteString] -> ByteString
renderCodeGenD = C8.unlines . map (C8.pack . render)
    where
    render c@DLabel{} = show c
    render c          = "  " <> show c

----------------------------------------------------

type CgT m e s a =
    EitherT e (
        ReaderT (Env s) (
            StateT (St s) m)) a

data Env s =
    Env { getKnownTopLevels :: Set s
        , foldedExpressions :: Map s (DVal s) -- TODO unused.  Can of worms?
        }

data St s =
    St { nextNum      :: !Int
       , varRegisters :: !(Map s R)
       }

type CgM m a =
    CgT m ByteString ByteString a


----------------------------------------------------

codeGenModuleD :: AnfModule ByteString
               -> Either ByteString [[DInstr ByteString]]
codeGenModuleD modu = runIdentity
                    . evalStateT' initState
                    . runReaderT' initEnv
                    . runEitherT
                    $ mapM codeGenFuncDWhole functions
    where
    functions = getFunDefAnfTs modu
    initEnv   = Env { getKnownTopLevels = S.fromList $ map (\(FunDefAnfT n _ _) -> n) functions
                    , foldedExpressions = mempty
                    }
    initState = St { nextNum = 0
                   , varRegisters = mempty :: Map ByteString R
                   }

-- Every block has a final location (vreg)
-- Hopefully we can remove the intermediate ones
-- A block may or may not have a label
data Block s =
    Block R [DInstr s]

-- Placeholder for when it gets real
codeGenFuncDWhole :: Monad m => FunDefAnfT ByteString -> CgM m [DInstr ByteString]
codeGenFuncDWhole f = do
    f' <- codeGenFuncD f
    -- Clean up excessive intermediate vregs
    -- Then colour
    -- Then add prologue & epilogue to preserve real registers in accordance with ABI
    pure f'

codeGenFuncD :: Monad m => FunDefAnfT ByteString -> CgM m [DInstr ByteString]
codeGenFuncD (FunDefAnfT name _ (AExp (ALam _ vs nexp))) = do

    -- Preserve variables shadowed by 'vs'
    regMap <- saveRegisterMap

    -- Pop in forward order
    pops <- forM vs $ \v ->
                DPop <$> bindFreshReg v

    -- First do the virtual register instructions
    Block r instrs <- codeGenNexp nexp

    -- Restore lexical scope (unshadow 'vs')
    restoreRegisterMap regMap

    -- This is just made up for now:
    pure $ concat [ [DLabel name]
                  , pops
                  , instrs
                  , [DRet r] ]

-- Top-level non-lambdas become lambdas
-- let's see how this bites us when we want top-level constants
codeGenFuncD (FunDefAnfT name q nexp) =
    codeGenFuncD (FunDefAnfT name q (AExp (ALam (typeOf nexp) [] nexp)))

codeGenNexp :: Monad m => NExp ByteString -> CgM m (Block ByteString)
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (CExp cexp)  = codeGenCexp cexp
codeGenNexp (NLet a b c) = codeGenNLet a b c

codeGenAexp :: Monad m => AExp ByteString -> CgM m (Block ByteString)
codeGenAexp (ATerm _ term) = codeGenTerm term

    where
    codeGenTerm (LitInt li) = do
        r <- freshReg
        let li' = fromIntegral li -- TODO tame this integral
        pure $ Block r [DMov (FromLitInt r li')]    -- this didn't really need a block

    codeGenTerm (Var v) =
        getRegister v >>= \case
            Nothing -> err [i|Not in scope #{v}|]
            Just r  -> pure $ Block r []    -- This didn't really need a block
        -- We could open this up to recognise top-level terms,
        -- as long as they've been folded down to expressions, i guess?
        -- otherwise this Aexp becomes too Cexp

    codeGenTerm v = do err [i|codeGenTerm #{v}|]

codeGenAexp ALam{} = err "Should not happen.  All lambdas already lifted"

codeGenAexp (ABinPrimOp _ op a b) = do

    {-  Important: if a or b is call (i.e. a variable a the top level)
        Then it really isn't an aexp!

        Maybe it should be fixed in the anf transform?

        Otherwise special cased right here
    -}

    Block ar ainstrs <- codeGenAexp a

    Block br binstrs <- codeGenAexp b

    r <- freshReg
    pure . Block r $ concat [ ainstrs
                            , binstrs
                            , [DBin r op' ar br] ]
    where
    op' =
        case op of
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

codeGenAexp aexp = err [i|codeGenAexp #{aexp}|]


-- A special kind of AExp which is hopefully a known label
--codeGenCall :: Monad m => AExp ByteString -> CgM m (DInstr ByteString)
--codeGenCall (ATerm _ (Var f)) = do
  --  known <- getKnownTopLevels <$> lift ask
    --if f `S.member` known
      --  then err "yay"
        --else err [i|Unrecognised term in call position #{f}|]


codeGenCexp :: Monad m => CExp ByteString -> CgM m (Block ByteString)

-- TODO: Remember to register the callgraph edge
codeGenCexp (CApp _ (ATerm _ (Var f)) xs) = do

    known <- getKnownTopLevels <$> lift ask

    unless (f `S.member` known)
           (err [i|Unrecognised term in call position #{f}|])

    -- Prepare and push the arguments
    xsBlocks <- mapM codeGenAexp xs
    let (rxs, xsinstrs) = unzip $ map (\(Block r is) -> (r, is)) xsBlocks
    let revPushes = map (DPush . DReg) (reverse rxs)

    r <- freshReg
    pure . Block r $ concat [ concat xsinstrs
                            , revPushes
                            , [ DCall (CallLabel f)
                              , DPop r ] ] -- known call

codeGenCexp aexp = err [i|codeGenCexp #{aexp}|]

codeGenNLet :: Monad m => ByteString
                       -> NExp ByteString
                       -> NExp ByteString
                       -> CgM m (Block ByteString)
codeGenNLet a b c = do

    -- Process b (before handling a - disallows recursive let)
    Block br binstrs <- codeGenNexp b

    -- Preserve lexical scope ('a' can shadow within here)
    regMap <- saveRegisterMap

    -- The variable 'a' refers to the final vreg of b
    register a br

    Block cr cinstrs <- codeGenNexp c

    -- Restore lexical scope
    restoreRegisterMap regMap

    let instrs = concat [ binstrs
                        , cinstrs ]

    pure (Block cr instrs)

---------------------------


saveRegisterMap :: Monad m => CgM m (Map ByteString R)
saveRegisterMap = lift $ lift (varRegisters <$> get)

restoreRegisterMap :: Monad m => Map ByteString R -> CgM m ()
restoreRegisterMap regMap = lift . lift . modify $ \s -> s { varRegisters = regMap }


{-

    Each *function* will adhere to ABI

    Each series of basic blocks (within function) will do register colouring (obeying any ABI requirements for outward calls)

-}


{-






codeGen :: AnfModule ByteString -> () -- Cg [[DInstr ByteString]]
codeGen modu = do
    funInstrs <- mapM codeGenFunDefn (getFunDefAnfTs modu)
    pure funInstrs

codeGenFunDefn :: FunDefAnfT ByteString
               -> Cg [DInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) = undefined



    Would like to calculate CFG (control-flow-graph)

        Perhaps Label shouldn't be an instruction.
            (Only conditionally introduce them later where CFG dictates?)

        Set (Edge from to)              -- 'from' and 'to' necessarily need block-IDs.
                                            -- 'to' can obviously be a (numbered) label/name
                                            -- 'from' should also be a (numbered) label

                                        -- Shouldn't be so bad that 'froms'
-}




{-
    codeGenNexp nexp <&> \(r, nexp') ->
        concat [ [DLabel name]
               , nexp'
               , [DRet r] ]


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

codeGenCApp :: Type ByteString
            -> AExp ByteString
            -> [AExp ByteString]
            -> Cg (DVal ByteString, [DInstr ByteString])
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

codeGenALam :: Type ByteString
            -> [ByteString]
            -> NExp ByteString
            -> Cg (DVal ByteString, [DInstr ByteString])
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

codeGenATerm :: Type ByteString
             -> Term ByteString
             -> Cg (DVal ByteString, [DInstr ByteString])
codeGenATerm _ (LitInt i)  = pure (DLitInt (fromIntegral i), []) -- TODO: tame fromintegral
codeGenATerm _ (LitBool b) = pure (DLitInt $ if b then 1 else 0, [])
codeGenATerm _ (Var v)     = mode2 -- Does complexity here imply that the ANF transform was done poorly?
    
    where
    mode2 = getRegister v <&> \case
        Just r  -> (DReg r, [])
        Nothing -> (DLbl v, []) -- Assuming lbl
    -- Old way was better?
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

getRegister :: Monad m => ByteString -> CgM m (Maybe R)
getRegister v = lift $ do
    vr <- varRegisters <$> lift get
    pure $ M.lookup v vr

bindFreshReg :: Monad m => ByteString -> CgM m R
bindFreshReg v = do
    fresh <- freshReg
    register v fresh
    pure fresh

register :: Monad m => ByteString -> R -> CgM m ()
register var reg = lift . lift . modify $ \s -> s { varRegisters = M.insert var reg (varRegisters s) }

freshReg :: Monad m => CgM m R
freshReg = R <$> freshNum

freshNum :: Monad m => CgM m Int
freshNum = lift . lift $ do
    gen <- get
    let rc = nextNum gen
    put gen { nextNum = rc + 1 }
    pure rc



--freshBranchLabel :: ByteString -> CgM m ByteString
--freshBranchLabel pre = freshNum <&> \n -> pre <> C8.pack (show n)
