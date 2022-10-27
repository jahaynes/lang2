{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen2 where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.CodeGen1

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map.Strict       (Map)
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding

data GenState =
    GenState { getRegNum    :: !Int
             , getBranchNum :: !Int }

newtype Reg =
    Reg ByteString
        deriving Show

data Stmt = Pop Reg
          | Push Param
          | Jmp Param
          | Cmp Param Param
          | Add Param Param
          | Sub Param Param
          | JmpNEq ByteString
          | Ret Val
          | Mov Param Param
          | Label ByteString
          | LoadFromStack Reg StackAddr
              deriving Show

renderCodeGen2 :: [Stmt] -> Text
renderCodeGen2 = T.unlines . map (T.pack . show)

codeGenModule2 :: [(ByteString, Val)] -> [Stmt]
codeGenModule2 = concatMap codeGen

    where
    codeGen (name, val) =
        Label name : evalState' (GenState 0 0) (emit val)

emit :: Val -> State GenState [Stmt]
emit val =
    
    case val of

        VFun si body ->
            -- Todo set up stack frame?
            emit body

        VIf pr tr fl -> do
            (then', else') <- freshThenElsePair
            pr' <- emit pr
            tr' <- emit tr
            fl' <- emit fl
            -- TODO jumps etc?
            pure $ concat [ pr'
                          , [JmpNEq else']
                          , [Label then']
                          -- done needed here?
                          , tr'
                          , [Label else']
                          , fl']

        VBinOp op v1 v2 -> do
            (is1, v1') <- asParam v1
            (is2, v2') <- asParam v2

            pure $ case op of
                       EqA  -> concat [is1, is2, [Cmp v1' v2']]
                       AddI -> concat [is1, is2, [Add v1' v2']]

        VBool{} ->
            pure [Ret val]

        VInt{} ->
            pure [Ret val]

        VStackValAt{} ->
            pure [Ret val]

        VApp addr args -> do
            (s1, dest)  <- asParam addr
            (s2, args') <- unzip <$> mapM asParam args
            let pushes = map Push args'
            pure $ concat [s1, concat s2, pushes, [Jmp dest]]

        VLet a b c -> do
            -- interleaving ok?
            (is1, a') <- asParam a -- isn't right.  is1 producing extra LoadFromStack with main()
            (is2, b') <- asParam b
            c' <- emit c
            pure $ concat [is1, is2, [Mov a' b'], c']

        _ ->
            error $ "unknown val: " ++ show val

data Param = PReg Reg
           | PImm Integer
           | PLabel ByteString
           | PResultReg
               deriving Show

asParam :: Val -> State GenState ([Stmt], Param)
asParam x =

    case x of

        VStackValAt v sa -> do
            reg <- freshReg
            pure ([LoadFromStack reg sa], PReg reg)

        VInt i -> do
            pure ([], PImm i)

        VBool b -> do
            let i = if b then 1 else 0 --- bool to num
            pure ([], PImm i)

        VBinOp op a b -> do
            (is1, a') <- asParam a
            (is2, b') <- asParam b
            pure $ case op of
                       AddI -> (concat [is1, is2, [Add a' b']], resultRegOf AddI)
                       SubI -> (concat [is1, is2, [Sub a' b']], resultRegOf SubI)

        VLabel lbl ->
            pure ([], PLabel lbl)

        _ -> error $ show x

resultRegOf :: BinOp -> Param
resultRegOf AddI = PResultReg
resultRegOf SubI = PResultReg

freshReg :: State GenState Reg
freshReg = do
    st <- get
    let num = getRegNum st
    put st { getRegNum = num + 1 }
    pure . Reg . pack $ "r" ++ show num

freshThenElsePair :: State GenState (ByteString, ByteString)
freshThenElsePair = do
    st <- get
    let num = getBranchNum st
    put st { getBranchNum = num + 1 }
    pure (pack $ "then_" ++ show num, pack $ "else_" ++ show num)