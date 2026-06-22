{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.Direct ( Asm (..)
                            , Code (..)
                            , Func (..)
                            , Label (..)
                            , codeGen
                            ) where

import Core.Expression
import Core.Module
import Core.Operator (BinOp, UnOp)
import Core.Term

import Common.State

import           Data.ByteString (ByteString)
import           Data.List       (nub, (\\))

-- | A label for jump targets and lifted closure code blocks.
data Label = Label Int
    deriving (Eq, Ord, Show)

-- | Simple, stack-machine style assembly-like instructions.
--
-- The model is a stack machine: an expression is compiled so that its
-- value is left on top of the stack.  Named locals (function parameters,
-- @let@-bound names, pattern variables) live in a named environment and
-- are accessed with 'Store' \/ 'Load'.
data Asm
    = PushInt Integer               -- ^ push an integer literal
    | PushBool Bool                 -- ^ push a boolean literal
    | PushStr ByteString            -- ^ push a string literal
    | PushVar ByteString            -- ^ push a global (top-level) reference
    | Load ByteString               -- ^ load a named local onto the stack
    | Store ByteString              -- ^ pop the stack and store into a named local
    | Pop                           -- ^ discard the top of the stack
    | MkClosure Label [ByteString]  -- ^ build a closure capturing the named locals
    | Apply Int                     -- ^ apply the closure on the stack to N args
    | Call ByteString               -- ^ direct call to a top-level function (args on stack)
    | Ret                           -- ^ return from a function / closure
    | MkData ByteString Int         -- ^ pack N fields from the stack into a constructor
    | Unpack Int                    -- ^ pop a data value, push its N fields
    | UnOp UnOp                     -- ^ apply a unary primitive operator
    | BinOp BinOp                   -- ^ apply a binary primitive operator
    | MkLabel Label                  -- ^ a code label (jump target)
    | Jump Label                    -- ^ unconditional jump
    | JumpIfFalse Label             -- ^ pop a bool, jump if it was false
    | MatchCtor ByteString Label    -- ^ peek top; jump if it is the named constructor
    | MatchInt Integer Label        -- ^ peek top; jump if it is the named integer
    | MatchBool Bool Label          -- ^ peek top; jump if it is the named boolean
    | MatchStr ByteString Label     -- ^ peek top; jump if it is the named string
    | Halt                          -- ^ stop execution (e.g. no pattern matched)
        deriving (Eq, Show)

-- | A compiled top-level function: its name, formal parameters and body.
data Func = Func ByteString [ByteString] [Asm]
    deriving (Eq, Show)

-- | A compiled module: the list of top-level functions together with any
-- closure bodies that were lifted out of them.  The closure bodies are
-- appended to each function's instruction stream and are reached via
-- 'MkClosure' \/ 'Label'.
data Code = Code [Func] [Asm]
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Code generation state

data GenState = GenState !Int [(Label, [Asm])]

type Gen = State GenState

initGen :: GenState
initGen = GenState 0 []

freshLabel :: Gen Label
freshLabel = do
    GenState n cs <- get
    put (GenState (n + 1) cs)
    pure (Label n)

-- | Record a lifted closure body (the body of a 'Lam') under a label.
addClosure :: Label -> [Asm] -> Gen ()
addClosure l body = modify' $ \(GenState n cs) -> GenState n ((l, body) : cs)

--------------------------------------------------------------------------------
-- Entry point

-- | Compile a 'Module' into assembly-like 'Code'.
--
-- Data definitions and type signatures carry no runtime code in this
-- simple model, so only the function definitions are lowered.  Symbol
-- names are specialised to 'ByteString' (as in the other back-end
-- phases); the type annotation @t@ is ignored.
codeGen :: Module t ByteString -> Either ByteString Code
codeGen (Module _ _ funDefns) =
    Right (Code (map compileFunDefn funDefns) [])

--------------------------------------------------------------------------------
-- Functions

compileFunDefn :: FunDefn t ByteString -> Func
compileFunDefn (FunDefn name quant body) =
    let params       = quantParams quant
        (instrs, st) = runState (compileExpr body) initGen
        GenState _ cs = st
        lifted       = concatMap (\(l, b) -> MkLabel l : b) (reverse cs)
    in Func name params (instrs ++ [Ret] ++ lifted)

quantParams :: Quant ByteString -> [ByteString]
quantParams (Quant ps) = ps
quantParams Unquant    = []

--------------------------------------------------------------------------------
-- Expressions
--
-- 'compileExpr' emits instructions that leave the value of the
-- expression on top of the stack.

compileExpr :: Expr t ByteString -> Gen [Asm]
compileExpr (Term _ term) =
    pure (compileTerm term)

-- A lambda becomes a closure.  Its free variables (minus the parameters)
-- are captured by name; the body is lifted out to a fresh label and
-- appended to the enclosing function's instruction stream.
compileExpr (Lam _ vs body) = do
    let captured = nub (freeVarsExpr body \\ vs)
    l         <- freshLabel
    bodyInstrs <- compileExpr body
    addClosure l (bodyInstrs ++ [Ret])
    pure [MkClosure l captured]

-- A saturated constructor application packs its fields; a top-level
-- function name is called directly; anything else is treated as a
-- closure value applied to its arguments.
compileExpr (App _ f xs) = case f of
    Term _ (DCons c) -> do
        argInstrs <- concat <$> mapM compileExpr xs
        pure (argInstrs ++ [MkData c (length xs)])
    Term _ (Var fn)  -> do
        argInstrs <- concat <$> mapM compileExpr xs
        pure (argInstrs ++ [Call fn])
    _                -> do
        fInstrs   <- compileExpr f
        argInstrs <- concat <$> mapM compileExpr xs
        pure (fInstrs ++ argInstrs ++ [Apply (length xs)])

compileExpr (Let _ name bind body) = do
    bindInstrs <- compileExpr bind
    bodyInstrs <- compileExpr body
    pure (bindInstrs ++ [Store name] ++ bodyInstrs)

compileExpr (UnPrimOp _ op a) = do
    aInstrs <- compileExpr a
    pure (aInstrs ++ [UnOp op])

compileExpr (BinPrimOp _ op a b) = do
    aInstrs <- compileExpr a
    bInstrs <- compileExpr b
    pure (aInstrs ++ bInstrs ++ [BinOp op])

compileExpr (IfThenElse _ pr tr fl) = do
    prInstrs  <- compileExpr pr
    lElse     <- freshLabel
    lEnd      <- freshLabel
    trInstrs  <- compileExpr tr
    flInstrs  <- compileExpr fl
    pure ( prInstrs
        ++ [JumpIfFalse lElse]
        ++ trInstrs
        ++ [Jump lEnd, MkLabel lElse]
        ++ flInstrs
        ++ [MkLabel lEnd] )

-- The scrutinee is stored in a temp local.  Each pattern is compiled to a
-- peek-and-jump test (leaving the scrutinee on the stack on mismatch); on
-- a mismatch a 'Pop' lets the next pattern be tried.  Pattern bodies are
-- collected separately and appended after all the tests.  If nothing
-- matches, 'Halt' is reached.
compileExpr (Case _ scrut ps) = do
    scrutInstrs <- compileExpr scrut
    let tmp = "__scrut"
    (matchInstrs, bodyBlocks, lEnd) <- compilePatterns tmp ps
    pure ( scrutInstrs
        ++ [Store tmp]
        ++ matchInstrs
        ++ [Halt]
        ++ concat bodyBlocks
        ++ [MkLabel lEnd] )

--------------------------------------------------------------------------------
-- Patterns

compilePatterns :: ByteString
                -> [Pattern t ByteString]
                -> Gen ([Asm], [[Asm]], Label)
compilePatterns tmp ps = do
    lEnd  <- freshLabel
    pairs <- mapM (compilePattern tmp lEnd) ps
    let (tests, bodies) = unzip pairs
    pure (concat tests, bodies, lEnd)

-- | Compile a single pattern.  Returns the test sequence (which jumps to
-- the pattern's body label on a match) and the body block (label + body +
-- jump to the end).
compilePattern :: ByteString
               -> Label
               -> Pattern t ByteString
               -> Gen ([Asm], [Asm])
compilePattern tmp lEnd (Pattern pat body) = do
    lBody      <- freshLabel
    bodyInstrs <- compileExpr body
    (test, consume, always) <- compilePatTest pat lBody
    let testSeq   = Load tmp : test ++ (if always then [] else [Pop])
        bodyBlock = MkLabel lBody : consume ++ bodyInstrs ++ [Jump lEnd]
    pure (testSeq, bodyBlock)

-- | Compile the match test for a single pattern shape.
--
-- Returns @(test, consume, always)@ where:
--
--   * @test@ peeks at the scrutinee (already on the stack) and jumps to
--     the body label on a match, falling through on a mismatch (leaving
--     the scrutinee on the stack).
--   * @consume@ is run at the body to consume the matched value (binding
--     any pattern variables).
--   * @always@ is 'True' for catch-all patterns that never fall through
--     (so no trailing 'Pop' is needed).
compilePatTest :: Expr t ByteString -> Label -> Gen ([Asm], [Asm], Bool)
compilePatTest pat lBody = case pat of
    Term _ (LitInt n)    -> pure ([MatchInt n lBody],    [Pop], False)
    Term _ (LitBool b)   -> pure ([MatchBool b lBody],   [Pop], False)
    Term _ (LitString s) -> pure ([MatchStr s lBody],    [Pop], False)
    Term _ (Var v)       -> pure ([Store v, Jump lBody], [],   True)
    Term _ (DCons c)     -> pure ([MatchCtor c lBody],   [Unpack 0], False)
    App _ (Term _ (DCons c)) argTerms ->
        let arity      = length argTerms
            bindInstrs = concatMap bindField (reverse argTerms)
        in pure ([MatchCtor c lBody], Unpack arity : bindInstrs, False)
    -- Anything else is treated as a catch-all.
    _ -> pure ([Jump lBody], [Pop], True)

-- | Bind a single constructor field: a variable pattern binds the value,
-- anything else (a nested pattern, unsupported here) is discarded.
bindField :: Expr t ByteString -> [Asm]
bindField (Term _ (Var v)) = [Store v]
bindField _                = [Pop]

--------------------------------------------------------------------------------
-- Terms

compileTerm :: Term ByteString -> [Asm]
compileTerm (Var s)       = [Load s]
compileTerm (DCons c)     = [MkData c 0]
compileTerm (LitInt n)    = [PushInt n]
compileTerm (LitBool b)   = [PushBool b]
compileTerm (LitString s) = [PushStr s]

--------------------------------------------------------------------------------
-- Free variables (for closure capture)
--
-- This is an over-approximation in the presence of pattern binders
-- (pattern variables are not subtracted), which is safe for capture: it
-- may capture a name that is shadow-bound at runtime, never omit one.

freeVarsExpr :: Expr t ByteString -> [ByteString]
freeVarsExpr (Term _ term)              = freeVarsTerm term
freeVarsExpr (Lam _ vs body)            = freeVarsExpr body \\ vs
freeVarsExpr (App _ f xs)               = freeVarsExpr f ++ concatMap freeVarsExpr xs
freeVarsExpr (Let _ name bind body)     = freeVarsExpr bind ++ (freeVarsExpr body \\ [name])
freeVarsExpr (UnPrimOp _ _ a)           = freeVarsExpr a
freeVarsExpr (BinPrimOp _ _ a b)        = freeVarsExpr a ++ freeVarsExpr b
freeVarsExpr (IfThenElse _ pr tr fl)    = freeVarsExpr pr ++ freeVarsExpr tr ++ freeVarsExpr fl
freeVarsExpr (Case _ scrut ps)          = freeVarsExpr scrut ++ concatMap freeVarsPat ps

freeVarsPat :: Pattern t ByteString -> [ByteString]
freeVarsPat (Pattern _ body) = freeVarsExpr body

freeVarsTerm :: Term ByteString -> [ByteString]
freeVarsTerm (Var _)       = []
freeVarsTerm (DCons _)     = []
freeVarsTerm (LitInt _)    = []
freeVarsTerm (LitBool _)   = []
freeVarsTerm (LitString _) = []
