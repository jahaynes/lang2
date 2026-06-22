{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             QuasiQuotes #-}

module Phase.CodeGen.CodeGenD ( codeGenModuleD
                              , renderCodeGenD
                              ) where

import Common.EitherT             (EitherT (..))
import Common.ReaderT             (runReaderT')
import Common.StateT              (evalStateT')
import Core.Operator
import Core.Term                  (Term (..))
import Phase.Anf.AnfExpression    (AExp (..), CExp (..), NExp (..), typeOf)
import Phase.Anf.AnfModule        (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.CodeGenDUtil (CgM, Env (..), St (..), bindFreshReg, err, freshReg, getRegister, register, restoreRegisterMap, saveRegisterMap)
import Phase.CodeGen.TypesD

import           Control.Monad               (forM)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor.Identity
import           Data.Map                    (Map)
import           Data.String.Interpolate     (i)

renderCodeGenD :: [DInstr ByteString] -> ByteString
renderCodeGenD = C8.unlines . map (C8.pack . render)
    where
    render c@DLabel{} = show c
    render c          = "  " <> show c

codeGenModuleD :: AnfModule ByteString
               -> Either ByteString [[DInstr ByteString]]
codeGenModuleD modu = runIdentity
                    . evalStateT' initState
                    . runReaderT' initEnv
                    . runEitherT
                    $ mapM codeGenFuncDWhole functions
    where
    functions = getFunDefAnfTs modu
    initEnv   = Env ()
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
    Block ar ainstrs <- codeGenAexp a
    Block br binstrs <- codeGenAexp b
    r                <- freshReg
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

codeGenCexp :: Monad m => CExp ByteString -> CgM m (Block ByteString)

-- TODO: Remember to register the callgraph edge
codeGenCexp (CApp _ (ATerm _ (Var f)) xs) = do

    -- Prepare and push the arguments
    xsBlocks <- mapM codeGenAexp xs
    let (rxs, xsinstrs) = unzip $ map (\(Block r is) -> (r, is)) xsBlocks
    let revPushes = map (DPush . DReg) (reverse rxs)

    r <- freshReg
    pure . Block r $ concat [ concat xsinstrs
                            , revPushes
                            , [ DCall (CallLabel f)
                              , DPop r ] ]

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
