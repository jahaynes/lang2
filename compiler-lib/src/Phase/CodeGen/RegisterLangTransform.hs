{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.RegisterLangTransform ( transformModule ) where

import           Common.EitherT (EitherT(..), left)
import           Common.State (State, evalState, get, modify')
import           Common.Trans (Trans(lift))
import           Core.Term (Term (..))
import           Core.Types (Type)
import           Phase.Anf.Anf
import           Phase.CodeGen.RegisterLang hiding (PPat, PVar, PApp)
import qualified Phase.Anf.Anf as Anf (PPat(..))
import qualified Phase.CodeGen.RegisterLang as Reg (PPat(..))

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.List             (zip3)
import           Data.Map              (Map)
import qualified Data.Map.Strict as M

---------------------------------------------------------------
-- Compilation state
---------------------------------------------------------------

data CompileState =
    CompileState { regCounter   :: !Int
                 , labelCounter :: !Int
                 , varMap       :: !(Map ByteString R)
                 }

initState :: CompileState
initState = CompileState 0 0 M.empty

type CompileM = EitherT ByteString (State CompileState)
---------------------------------------------------------------
-- Fresh name generation
---------------------------------------------------------------

freshReg :: CompileM R
freshReg = do
    n <- lift (regCounter <$> get)
    lift $ modify' $ \s -> s { regCounter = n + 1 }
    pure (R n)

freshLabel :: CompileM L
freshLabel = do
    n <- lift (labelCounter <$> get)
    lift $ modify' $ \s -> s { labelCounter = n + 1 }
    pure (L n)

lookupVar :: ByteString -> CompileM R
lookupVar v = do
    m <- lift (varMap <$> get)
    case M.lookup v m of
        Just r  -> pure r
        Nothing -> left $ pack $ "RegisterLangTransform: unbound variable " ++ show v

bindVar :: ByteString -> R -> CompileM ()
bindVar v r = do
    m <- lift (varMap <$> get)
    lift $ modify' $ \s -> s { varMap = M.insert v r m }

---------------------------------------------------------------
-- Conversions between ANF and RegisterLang types
---------------------------------------------------------------

aexpToATerm :: AExp ByteString -> CompileM (ATerm ByteString)
aexpToATerm (ATerm _ (Var v))     = AVar <$> lookupVar v
aexpToATerm (ATerm _ (DCons c))   = pure (ADCons c)
aexpToATerm (ATerm _ (LitInt n))  = pure (ALitInt n)
aexpToATerm (ATerm _ (LitBool b)) = pure (ALitBool b)
aexpToATerm (ATerm _ (LitString s)) = pure (ALitString s)

aexpType :: AExp ByteString -> Type ByteString
aexpType (ATerm t _) = t

anfPatToRegPat :: Anf.PPat ByteString -> Reg.PPat ByteString
anfPatToRegPat (Anf.PVar v)       = Reg.PVar v
anfPatToRegPat (Anf.PApp c t ps)  = Reg.PApp c t (map anfPatToRegPat ps)
---------------------------------------------------------------
-- Top-level module transformation
---------------------------------------------------------------

transformModule :: AnfModule ByteString -> Either ByteString (RegModule ByteString)
transformModule (AnfModule dds fds) = do
    funDefs <- mapM transformFunDef fds
    pure $ RegModule dds funDefs

---------------------------------------------------------------
-- Function definition transformation
---------------------------------------------------------------

transformFunDef :: FunDefAnfT ByteString -> Either ByteString (RegFunDef ByteString)
transformFunDef (FunDefAnfT name quant ty envVars params body) =
    case evalState (runEitherT (compileBody name envVars params body)) initState of
        Left err      -> Left err
        Right blocks  -> Right $ RegFunDef name quant ty (length envVars) (length params) blocks

-- | Compile the body of a function definition into basic blocks.
--   The parameters are bound to registers, then the body is compiled.
--   The final block ends with a Return terminator.
compileBody :: ByteString -> [ByteString] -> [ByteString] -> NExp ByteString -> CompileM [Block ByteString]
compileBody _name envVars params body = do
    -- Bind environment variables to registers
    envRegs <- mapM (\_ -> freshReg) envVars
    mapM_ (uncurry bindVar) (zip envVars envRegs)
    -- Bind parameters to registers
    paramRegs <- mapM (\_ -> freshReg) params
    mapM_ (uncurry bindVar) (zip params paramRegs)
    -- Compile the body: the final result is returned, so we use Nothing for the
    -- continuation label, which signals that the last block should end with Return.
    (blocks, _) <- compileNExp body Nothing
    pure blocks

---------------------------------------------------------------
-- NExp compilation
---------------------------------------------------------------

-- | Compile an NExp.
--   When 'mcont' is 'Nothing', the expression is the top-level function body
--   and the last block uses 'Return'.  When 'mcont' is 'Just cont', the
--   expression is a sub-expression whose result must be written to the
--   destination register and then jump to the continuation label.
--
--   Returns the blocks and the entry label.
compileNExp :: NExp ByteString -> Maybe L -> CompileM ([Block ByteString], L)
compileNExp nexp mcont = go nexp
  where
    go :: NExp ByteString -> CompileM ([Block ByteString], L)
    go (AExp a) = do
        r      <- freshReg
        lbl    <- freshLabel
        let t = aexpType a
        let term = case mcont of
                Nothing     -> Return t [r]
                Just contL  -> Jump contL
        aTerm <- aexpToATerm a
        pure ([Block lbl [Move t r aTerm] term], lbl)

    go (NLet _ s e1 e2) = do
        -- Pre-allocate a register for the bound variable
        r_s <- freshReg
        bindVar s r_s
        -- Create a continuation label for e1 (the entry point of e2)
        midL <- freshLabel
        -- Compile e1: result goes to r_s, then jump to midL
        (blocks1, _l1) <- compileNExpTo e1 r_s (Just midL)
        -- Compile e2: result goes to the final destination
        (blocks2, _l2) <- go e2
        -- Re-label the first block of e2 to midL so the jump from e1 lands correctly
        let blocks2' = case blocks2 of
                []      -> [Block midL [] (Jump midL)]  -- should not happen
                (b:bs)  -> Block midL (getInsts b) (getTerm b) : bs
        pure (blocks1 ++ blocks2', midL)

    go (CExp c) = compileCExp c mcont
-- | Like 'compileNExp', but the result register is predetermined.
--   The continuation label (when 'Just') is where to jump after the value
--   is computed.
compileNExpTo :: NExp ByteString -> R -> Maybe L -> CompileM ([Block ByteString], L)
compileNExpTo nexp rDest mcont = case nexp of
    AExp a -> do
        lbl <- freshLabel
        let t = aexpType a
        let term = case mcont of
                Nothing     -> Return t [rDest]
                Just contL  -> Jump contL
        aTerm <- aexpToATerm a
        pure ([Block lbl [Move t rDest aTerm] term], lbl)

    NLet _ s e1 e2 -> do
        -- Pre-allocate a register for the bound variable
        r_s <- freshReg
        bindVar s r_s
        -- Create a continuation label for e1
        midL <- freshLabel
        (blocks1, _l1) <- compileNExpTo e1 r_s (Just midL)
        (blocks2, _l2) <- compileNExpTo e2 rDest mcont
        -- Re-label the first block of e2 to midL so the jump from e1 lands correctly
        let blocks2' = case blocks2 of
                []      -> [Block midL [] (Jump midL)]  -- should not happen
                (b:bs)  -> Block midL (getInsts b) (getTerm b) : bs
        pure (blocks1 ++ blocks2', midL)

    CExp c -> compileCExpTo c rDest mcont

---------------------------------------------------------------
-- CExp compilation
---------------------------------------------------------------

-- | Compile a CExp.  The result is written to a fresh register and the
--   continuation is either 'Return' or 'Jump' depending on mcont.
compileCExp :: CExp ByteString -> Maybe L -> CompileM ([Block ByteString], L)
compileCExp cexp mcont = do
    rDest <- freshReg
    compileCExpTo cexp rDest mcont

-- | Compile a CExp to a predetermined destination register.
compileCExpTo :: CExp ByteString -> R -> Maybe L -> CompileM ([Block ByteString], L)
compileCExpTo cexp rDest mcont = case cexp of

    ---------------------------------------------------------------
    -- Unary primitive operation
    ---------------------------------------------------------------
    CUnPrimOp t op a -> do
        r_a <- freshReg
        lbl <- freshLabel
        let term = case mcont of
                Nothing     -> Return t [rDest]
                Just contL  -> Jump contL
        aTerm <- aexpToATerm a
        let insts = [ Move t r_a aTerm
                    , UnOp t rDest op r_a
                    ]
        pure ([Block lbl insts term], lbl)

    ---------------------------------------------------------------
    -- Binary primitive operation
    ---------------------------------------------------------------
    CBinPrimOp t op a1 a2 -> do
        r1 <- freshReg
        r2 <- freshReg
        lbl <- freshLabel
        let term = case mcont of
                Nothing     -> Return t [rDest]
                Just contL  -> Jump contL
        a1Term <- aexpToATerm a1
        a2Term <- aexpToATerm a2
        let insts = [ Move t r1 a1Term
                    , Move t r2 a2Term
                    , BinOp t rDest op r1 r2
                    ]
        pure ([Block lbl insts term], lbl)

    ---------------------------------------------------------------
    -- If-then-else
    ---------------------------------------------------------------
    CIfThenElse t c th el -> do
        r_c     <- freshReg
        l_cond  <- freshLabel
        l_then  <- freshLabel
        l_else  <- freshLabel
        l_join  <- freshLabel
        cTerm   <- aexpToATerm c

        -- The condition block: load the condition and branch
        let condBlock = Block l_cond
                            [Move t r_c cTerm]
                            (Branch t r_c l_then l_else)

        -- Compile the 'then' branch: result goes to rDest, then jump to l_join
        (thenBlocks, _lt) <- compileNExpTo th rDest (Just l_join)
        -- Compile the 'else' branch: result goes to rDest, then jump to l_join
        (elseBlocks, _le) <- compileNExpTo el rDest (Just l_join)

        -- The join block: jump to the outer continuation, or return
        let joinTerm = case mcont of
                Nothing    -> Return t [rDest]
                Just contL -> Jump contL
        let joinBlock = Block l_join [] joinTerm

        pure (condBlock : thenBlocks ++ elseBlocks ++ [joinBlock], l_cond)
---------------------------------------------------------------
    -- Direct function call
    ---------------------------------------------------------------
    CApp t f args -> do
        -- Extract the function name from the atomic expression
        funcName <- case f of
                ATerm _ (Var n) -> pure n
                _ -> left $ pack "RegisterLangTransform.CApp: function must be a Var"
        -- Load arguments into registers
        r_args <- mapM (\_ -> freshReg) args
        l_call <- freshLabel
        l_ret  <- freshLabel

        argTerms <- mapM aexpToATerm args

        let argInsts = [Move (aexpType a) r t
                       | (a, r, t) <- zip3 args r_args argTerms]

        let retBlock = case mcont of
                Nothing    -> Block l_ret [] (Return t [rDest])
                Just contL -> Block l_ret [] (Jump contL)

        pure ( [Block l_call argInsts (Call t [rDest] funcName r_args l_ret)]
              ++ [retBlock]
             , l_call
             )

    ---------------------------------------------------------------
    -- Closure call (calls a lifted function with its captured env vars)
    ---------------------------------------------------------------
    CAppClo t f (AClosEnv env) args -> do
        -- Extract the function name from the atomic expression.
        -- In the ANF, CAppClo always has a Var for the function.
        funcName <- case f of
                ATerm _ (Var n) -> pure n
                _ -> left $ pack "RegisterLangTransform.CAppClo: function must be a Var"
        -- Look up the registers for the captured environment variables
        r_env <- mapM lookupVar env
        -- Load the arguments into registers
        r_args <- mapM (\_ -> freshReg) args
        l_call <- freshLabel
        l_ret  <- freshLabel

        argTerms <- mapM aexpToATerm args

        let argInsts = [Move (aexpType a) r t
                       | (a, r, t) <- zip3 args r_args argTerms]

        let retBlock = case mcont of
                Nothing    -> Block l_ret [] (Return t [rDest])
                Just contL -> Block l_ret [] (Jump contL)

        -- Direct call: env vars (already in registers) precede regular args
        pure ( [Block l_call argInsts (Call t [rDest] funcName (r_env ++ r_args) l_ret)]
              ++ [retBlock]
             , l_call
             )

    ---------------------------------------------------------------
    -- Case / pattern matching
    ---------------------------------------------------------------
    CCase t scrut alts -> do
        r_s <- freshReg
        l_case <- freshLabel
        l_default <- freshLabel
        scrutTerm <- aexpToATerm scrut

        -- Compile each alternative body and collect (pattern, label) pairs
        (altBlocks, altPats) <- compileAlts rDest mcont alts

        -- The scrutinee block: load the scrutinee and dispatch with Case
        let scrutBlock = Block l_case
                            [Move t r_s scrutTerm]
                            (Case t r_s altPats l_default)

        -- Default block (unmatched pattern): just jump to the continuation
        let defaultTerm = case mcont of
                Nothing    -> Return t [rDest]
                Just contL -> Jump contL
        let defaultBlock = Block l_default [] defaultTerm

        pure (scrutBlock : altBlocks ++ [defaultBlock], l_case)

---------------------------------------------------------------
-- Case alternative compilation
---------------------------------------------------------------

-- | Compile the list of case alternatives, returning the blocks (in order)
--   and the (pattern, label) pairs for the Case terminator.
compileAlts :: R -> Maybe L -> [PExp ByteString]
           -> CompileM ([Block ByteString], [(Reg.PPat ByteString, L)])
compileAlts rDest mcont alts = do
    results <- mapM (compileSingleAlt rDest mcont) alts
    pure (concatMap fst results, concatMap snd results)

compileSingleAlt :: R -> Maybe L -> PExp ByteString
                -> CompileM ([Block ByteString], [(Reg.PPat ByteString, L)])
compileSingleAlt rDest mcont (PExp pat body) = do
    l_alt <- freshLabel
    (blocks, _l_entry) <- compileNExpTo body rDest mcont
    -- The entry block must have the label l_alt, so we rebind it
    let blocks' = case blocks of
            [] -> [Block l_alt [] (Jump l_alt)]  -- should not happen
            (b:bs) -> Block l_alt (getInsts b) (getTerm b) : bs
    pure (blocks', [(anfPatToRegPat pat, l_alt)])

-- | Extract the instructions from a block.
getInsts :: Block s -> [Inst s]
getInsts (Block _ insts _) = insts

-- | Extract the terminator from a block.
getTerm :: Block s -> Terminator s
getTerm (Block _ _ term) = term