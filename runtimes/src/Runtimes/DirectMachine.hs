{-# LANGUAGE OverloadedStrings,
             QuasiQuotes #-}

module Runtimes.DirectMachine ( Value (..)
                              , ExecResult
                              , runCode
                              , runMain
                              , showValue
                              ) where

import Core.Operator
import Phase.CodeGen.Direct

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as C8
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Set              (Set)
import qualified Data.Set             as Set
import           Data.String.Interpolate (i)
import           Data.Vector           (Vector)
import qualified Data.Vector           as V

--------------------------------------------------------------------------------
-- Runtime values

-- | A value inhabiting the machine's operand stack.
data Value
    = VInt  !Integer              -- ^ integer literal
    | VBool !Bool                 -- ^ boolean literal
    | VStr  !ByteString           -- ^ string literal
    | VData !ByteString !(Vector Value)
                                  -- ^ a packed constructor application
    | VClosure !Label !(Map ByteString Value) ![ByteString]
                                  -- ^ a lambda closure: its body label, the
                                  -- captured environment and its formal
                                  -- parameters
    | VFunc !ByteString            -- ^ a reference to a top-level function
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Frames and the machine

-- | A single execution frame.  Each frame owns its own operand stack so
-- that values left on the stack across a call are preserved; the Haskell
-- call stack plays the role of the machine call stack (a saved frame is
-- resumed when the callee returns).
data Frame = Frame
    { fCode :: !(Vector Asm)          -- ^ instruction stream
    , fPc   :: !Int                   -- ^ program counter
    , fLocs :: !(Map ByteString Value) -- ^ named-local environment
    , fStk  :: ![Value]               -- ^ operand stack
    , fLbls :: !(Map Label Int)       -- ^ label -> index map
    }

-- | The code of a callable (top-level function or lifted closure body):
-- its formal parameters, instruction stream and label map.
data ClosureCode = ClosureCode
    { ccParams :: ![ByteString]
    , ccBody   :: !(Vector Asm)
    , ccLbls   :: !(Map Label Int)
    }

-- | The result of running a program: either the final value or a runtime
-- error message.
type ExecResult = Either String Value

--------------------------------------------------------------------------------
-- Entry points

-- | Run a compiled 'Code' unit, executing its @main@ function with no
-- arguments and returning the result.
runMain :: Code -> ExecResult
runMain code@(Code funs _) =
    case lookupFunc funs "main" of
        Nothing      -> Left "runtime error: no 'main' function"
        Just closure -> runCode' funs Map.empty (enterFunc closure [])

-- | Run a compiled 'Code' unit from an arbitrary starting frame.
runCode :: Code -> Frame -> ExecResult
runCode (Code funs _) = runCode' funs Map.empty

--------------------------------------------------------------------------------
-- Internals

-- | Build the global table of top-level functions.
buildGlobals :: [Func] -> Map ByteString ClosureCode
buildGlobals = Map.fromList . map mkGlobal
  where
    mkGlobal (Func name params body) =
        (name, ClosureCode params (V.fromList body) (buildLabels body))

-- | Index every 'MkLabel' in an instruction stream (the index of the
-- 'MkLabel' itself, so a jump lands on it).
buildLabels :: [Asm] -> Map Label Int
buildLabels = go 0 Map.empty
  where
    go _ acc []              = acc
    go i acc (MkLabel l : rest) = go (i + 1) (Map.insert l i acc) rest
    go i acc (_ : rest)       = go (i + 1) acc rest

-- | Look up a top-level function by name.
lookupFunc :: [Func] -> ByteString -> Maybe ClosureCode
lookupFunc funs name =
    case [ f | f@(Func n _ _) <- funs, n == name ] of
        (Func _ params body : _) -> Just (ClosureCode params (V.fromList body) (buildLabels body))
        []                       -> Nothing

-- | Enter a top-level function body, binding the supplied arguments to its
-- formal parameters.
enterFunc :: ClosureCode -> [Value] -> Frame
enterFunc (ClosureCode params body lbls) args =
    Frame body 0 (Map.fromList (zip params args)) [] lbls

-- | Enter a lifted closure body at its label, binding the formal
-- parameters and overlaying the captured environment.
enterClosure :: ClosureCode -> Label -> Map ByteString Value -> [ByteString] -> [Value] -> Frame
enterClosure cc l captured params args =
    let env = Map.union (Map.fromList (zip params args)) captured
        pc  = Map.findWithDefault 0 l (ccLbls cc)
    in Frame (ccBody cc) pc env [] (ccLbls cc)

--------------------------------------------------------------------------------
-- The interpreter loop

-- | The main interpreter loop.  The machine's call stack is the Haskell
-- call stack: 'doCall'/'doApply' recurse into the callee, and 'retFrom'
-- returns a value to the caller (which is the caller of 'runCode').
runCode' :: [Func]
         -> Map ByteString ClosureCode
         -> Frame
         -> ExecResult
runCode' funs globals0 frame0 =
    let globals = if Map.null globals0 then buildGlobals funs else globals0
    in go globals frame0
  where
    go globals frame
        | fPc frame >= V.length (fCode frame) = retFrom frame
        | otherwise =
            case fCode frame V.!? fPc frame of
                Nothing  -> retFrom frame
                Just asm -> step globals frame asm

    step globals frame asm =
        let pc = fPc frame + 1 in
        case asm of
            PushInt n       -> go globals frame { fPc = pc, fStk = VInt n  : fStk frame }
            PushBool b      -> go globals frame { fPc = pc, fStk = VBool b : fStk frame }
            PushStr s       -> go globals frame { fPc = pc, fStk = VStr s   : fStk frame }
            PushVar v       -> go globals frame { fPc = pc, fStk = VFunc v : fStk frame }
            Load n          ->
                case Map.lookup n (fLocs frame) of
                    Just v  -> go globals frame { fPc = pc, fStk = v : fStk frame }
                    Nothing ->
                        -- Not a local; try it as a top-level function reference
                        go globals frame { fPc = pc, fStk = VFunc n : fStk frame }
            Store n         ->
                case fStk frame of
                    (v : rest) -> go globals frame { fPc = pc
                                                   , fLocs = Map.insert n v (fLocs frame)
                                                   , fStk = rest }
                    []          -> Left "runtime error: Store on empty stack"
            Pop             ->
                case fStk frame of
                    (_ : rest) -> go globals frame { fPc = pc, fStk = rest }
                    []          -> Left "runtime error: Pop on empty stack"
            MkClosure l captured params ->
                let env = Map.restrictKeys (fLocs frame) (Set.fromList captured)
                in go globals frame { fPc = pc, fStk = VClosure l env params : fStk frame }
            Apply n         -> doApply globals frame pc n
            Call fn         -> doCall globals frame pc fn
            Ret             -> retFrom frame
            MkData c k      ->
                case popN (fStk frame) k of
                    Nothing              -> Left "runtime error: MkData underflow"
                    Just (fields, rest)  -> go globals frame { fPc = pc
                                                              , fStk = VData c (V.fromList fields) : rest }
            Unpack k        ->
                case fStk frame of
                    (VData _ fs : rest) ->
                        if k == 0 || k == V.length fs
                            then go globals frame { fPc = pc, fStk = V.toList fs ++ rest }
                            else Left "runtime error: Unpack arity mismatch"
                    (v : _) -> Left ("runtime error: Unpack on non-data value: " ++ show v)
                    []      -> Left "runtime error: Unpack on empty stack"
            UnOp op         ->
                case fStk frame of
                    (a : rest) -> do v <- evalUnOp op a
                                     go globals frame { fPc = pc, fStk = v : rest }
                    []          -> Left "runtime error: UnOp on empty stack"
            BinOp op        ->
                case fStk frame of
                    (b : a : rest) -> do v <- evalBinOp op a b
                                         go globals frame { fPc = pc, fStk = v : rest }
                    _              -> Left "runtime error: BinOp underflow"
            MkLabel _       -> go globals frame { fPc = pc }
            Jump l          -> jump globals frame l
            JumpIfFalse l   ->
                case fStk frame of
                    (VBool b : rest) ->
                        if b then go globals frame { fPc = pc, fStk = rest }
                             else jump globals frame { fStk = rest } l
                    (v : _) -> Left ("runtime error: JumpIfFalse on non-bool: " ++ show v)
                    []      -> Left "runtime error: JumpIfFalse on empty stack"
            MatchCtor c l   ->
                case fStk frame of
                    (VData c' _ : _) -> if c == c'
                                           then jump globals frame l
                                           else go globals frame { fPc = pc }
                    (_ : _) -> go globals frame { fPc = pc }
                    []      -> Left "runtime error: MatchCtor on empty stack"
            MatchInt n l    ->
                case fStk frame of
                    (VInt n' : _) -> if n == n'
                                        then jump globals frame l
                                        else go globals frame { fPc = pc }
                    (_ : _) -> go globals frame { fPc = pc }
                    []      -> Left "runtime error: MatchInt on empty stack"
            MatchBool b l   ->
                case fStk frame of
                    (VBool b' : _) -> if b == b'
                                         then jump globals frame l
                                         else go globals frame { fPc = pc }
                    (_ : _) -> go globals frame { fPc = pc }
                    []      -> Left "runtime error: MatchBool on empty stack"
            MatchStr s l    ->
                case fStk frame of
                    (VStr s' : _) -> if s == s'
                                        then jump globals frame l
                                        else go globals frame { fPc = pc }
                    (_ : _) -> go globals frame { fPc = pc }
                    []      -> Left "runtime error: MatchStr on empty stack"
            Halt             -> Left "runtime error: non-exhaustive patterns (Halt reached)"

    jump globals frame l =
        case Map.lookup l (fLbls frame) of
            Just i  -> go globals frame { fPc = i }
            Nothing -> Left [i|runtime error: jump to unknown label: #{l}|]

    -- | Apply the callable on the stack to N arguments.  The args and the
    -- callable are popped; the callee is entered recursively.  If the
    -- callable receives more arguments than its arity, the result is
    -- itself applied to the remaining arguments (currying).
    doApply globals frame pc n =
        case popN (fStk frame) n of
            Nothing -> Left "runtime error: Apply underflow"
            Just (args, rest) ->
                case rest of
                    (f : rest') -> applyValue globals frame pc rest' f args
                    [] -> Left "runtime error: Apply with no callable"

    -- | Repeatedly apply a callable value to arguments, currying when there
    -- are more arguments than the callable's arity.
    applyValue globals frame pc rest f args = case f of
        VFunc name ->
            case lookupFunc funs name of
                Just cc ->
                    let arity = length (ccParams cc)
                        (direct, restArgs) = splitAt arity args
                    in if null restArgs
                       then do v <- go globals (enterFunc cc direct)
                               go globals frame { fPc = pc, fStk = v : rest }
                       else do v <- go globals (enterFunc cc direct)
                               applyValue globals frame pc rest v restArgs
                Nothing -> Left ("runtime error: unknown function " ++ C8.unpack name)
        VClosure l env params ->
            let arity = length params
                (direct, restArgs) = splitAt arity args
            in case findClosureCode globals l of
                Just cc ->
                    if null restArgs
                    then do v <- go globals (enterClosure cc l env params direct)
                            go globals frame { fPc = pc, fStk = v : rest }
                    else do v <- go globals (enterClosure cc l env params direct)
                            applyValue globals frame pc rest v restArgs
                Nothing -> Left "runtime error: closure label not found"
        _ -> Left ("runtime error: cannot apply non-function value: " ++ show f)

    -- | Direct call to a named function.  The arguments are already on the
    -- stack; the callee is entered recursively and its result replaces the
    -- argument block on the caller's stack.
    doCall globals frame pc fn =
        case callableArity globals (fLocs frame) fn of
            Right argCount ->
                case popN (fStk frame) argCount of
                    Nothing -> Left "runtime error: Call underflow"
                    Just (args, rest) ->
                        case buildCallee globals (fLocs frame) fn args of
                            Right callee -> do v <- go globals callee
                                               go globals frame { fPc = pc, fStk = v : rest }
                            Left e       -> Left e
            Left e -> Left e

    -- | Get the argument count for a callable by name.
    callableArity globals locs fn =
        case Map.lookup fn locs of
            Just (VClosure _ _ params) -> Right (length params)
            Just (VFunc name)          -> case lookupFunc funs name of
                Just cc -> Right (length (ccParams cc))
                Nothing -> Left ("runtime error: unknown function " ++ C8.unpack name)
            _                          -> case lookupFunc funs fn of
                Just cc -> Right (length (ccParams cc))
                Nothing -> Left ("runtime error: unknown function " ++ C8.unpack fn)

    -- | Build the callee frame for a named callable, binding the supplied
    -- arguments to its formal parameters.
    buildCallee globals locs fn args =
        case Map.lookup fn locs of
            Just (VClosure l env params) ->
                case findClosureCode globals l of
                    Just cc -> Right (enterClosure cc l env params args)
                    Nothing -> Left "runtime error: closure label not found"
            Just (VFunc name) ->
                case lookupFunc funs name of
                    Just cc -> Right (enterFunc cc args)
                    Nothing -> Left ("runtime error: unknown function " ++ C8.unpack name)
            _ ->
                case lookupFunc funs fn of
                    Just cc -> Right (enterFunc cc args)
                    Nothing -> Left ("runtime error: unknown function " ++ C8.unpack fn)

    -- | Dispatch a value-as-callable to its arguments.
    dispatch globals f args = case f of
        VClosure l env params ->
            case findClosureCode globals l of
                Just cc -> Right (enterClosure cc l env params args)
                Nothing -> Left "runtime error: closure label not found"
        VFunc name ->
            case lookupFunc funs name of
                Just cc -> Right (enterFunc cc args)
                Nothing -> Left ("runtime error: unknown function " ++ C8.unpack name)
        _ -> Left ("runtime error: cannot apply non-function value: " ++ show f)

    -- | Find the 'ClosureCode' (function body + label map) that contains a
    -- given lifted-closure label.  Closure bodies are appended to their
    -- enclosing top-level function's instruction stream, so we search the
    -- global table for a function whose label map mentions @l@.
    findClosureCode globals l = goFind (Map.elems globals)
      where
        goFind [] = Nothing
        goFind (cc : ccs)
            | Map.member l (ccLbls cc) = Just cc
            | otherwise                = goFind ccs

    retFrom frame =
        case fStk frame of
            (v : _) -> Right v
            []      -> Left "runtime error: Ret with empty stack"

-- | Pop @n@ values off the top of the stack, returning them in left-to-right
-- (push) order together with the remaining stack.
--
-- The stack is a list whose head is the top.  Values are pushed left-to-right,
-- so the rightmost argument is on top.  We pop @n@ values off the top
-- (collecting them by prepending to the accumulator) and return the
-- accumulator directly: because each newly-popped value is prepended, after
-- popping all @n@ of them the accumulator is in push order
-- @[x1, ..., xn]@ (leftmost-pushed first).  Reversing it (as a previous
-- version did) would instead yield top-first @[[xn, ..., x1]@, swapping
-- constructor-field and call-argument order.
popN :: [a] -> Int -> Maybe ([a], [a])
popN xs n = go [] n xs
  where
    go acc n xs | n == 0 = Just (acc, xs)
    go acc n (x : xs)   = go (x : acc) (n - 1) xs
    go _   n []         = Nothing

--------------------------------------------------------------------------------
-- Primitive operators

evalUnOp :: UnOp -> Value -> Either String Value
evalUnOp Negate (VInt n)  = Right (VInt (negate n))
evalUnOp Negate v         = Left ("runtime error: Negate on non-int: " ++ show v)
evalUnOp EShow   v        = Right (VStr (showValue v))
evalUnOp Err     v        = Left [i|runtime error: explicit error: #{showValue v}|]

evalBinOp :: BinOp -> Value -> Value -> Either String Value
evalBinOp AddI  (VInt a)  (VInt b)  = Right (VInt (a + b))
evalBinOp SubI  (VInt a)  (VInt b)  = Right (VInt (a - b))
evalBinOp MulI  (VInt a)  (VInt b)  = Right (VInt (a * b))
evalBinOp DivI  (VInt _)  (VInt 0)  = Left "runtime error: division by zero"
evalBinOp DivI  (VInt a)  (VInt b)  = Right (VInt (a `div` b))
evalBinOp ModI  (VInt _)  (VInt 0)  = Left "runtime error: modulo by zero"
evalBinOp ModI  (VInt a)  (VInt b)  = Right (VInt (a `mod` b))
evalBinOp EqA   a         b         = Right (VBool (a == b))
evalBinOp LtEqI (VInt a)  (VInt b)  = Right (VBool (a <= b))
evalBinOp LtI   (VInt a)  (VInt b)  = Right (VBool (a < b))
evalBinOp GtEqI (VInt a)  (VInt b)  = Right (VBool (a >= b))
evalBinOp GtI   (VInt a)  (VInt b)  = Right (VBool (a > b))
evalBinOp AndB  (VBool a) (VBool b) = Right (VBool (a && b))
evalBinOp OrB   (VBool a) (VBool b) = Right (VBool (a || b))
evalBinOp ConcatS (VStr a) (VStr b) = Right (VStr (a <> b))
evalBinOp op    a         b         = Left ("runtime error: bad operands for "
                                          ++ show op ++ ": " ++ show a ++ ", " ++ show b)

-- | Render a value as a string (used by 'EShow' and error messages).
showValue :: Value -> ByteString
showValue (VInt n)     = C8.pack (show n)
showValue (VBool b)    = if b then "true" else "false"
showValue (VStr s)     = s
showValue (VData c fs) = C8.unwords (c : map showValue (V.toList fs))
showValue v            = C8.pack (show v)
