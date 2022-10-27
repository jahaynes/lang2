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

{-
data Val = VFun StackInfo Val
         | VUnOp UnOp Val
         | VBinOp BinOp Val Val
         | VStackValAt ByteString StackAddr
         | VApp Val [Val]
         | VIf Val Val Val
         | VBool Bool
         | VInt Integer
         | VLabel ByteString
         | VLet Val Val Val
             deriving Show
-}

newtype GenState =
    GenState { getRegNum :: Int }

newtype Reg =
    Reg ByteString
        deriving Show

data Stmt = Pop ByteString -- reg?
          | Push ByteString -- reg?
          | Jmp ByteString
          | Cmp Param Param
          | JmpEq ByteString
          | Ret Val
          | Label ByteString
          | LoadFromStack Reg StackAddr
              deriving Show

renderCodeGen2 :: [Stmt] -> Text
renderCodeGen2 = T.unlines . map (T.pack . show)

codeGenModule2 :: [(ByteString, Val)] -> [Stmt]
codeGenModule2 = concatMap codeGen

    where
    codeGen (name, val) =
        Label name : evalState' (GenState 0) (emit val)

emit :: Val -> State GenState [Stmt]
emit val =
    
    case val of

        VFun si body ->
            -- Todo set up stack frame?
            emit body

        VIf pr tr fl -> do
            pr' <- emit pr
            tr' <- emit tr
            fl' <- emit fl
            -- TODO jumps etc?
            pure $ concat [pr', tr', fl']

        VBinOp EqA v1 v2 -> do
            (is1, v1') <- asParam v1
            (is2, v2') <- asParam v2
            pure $ concat [is1, is2, [Cmp v1' v2']]

        VBool{} ->
            pure [Ret val]

        VApp (VLabel addr) args -> do
            -- do something with args
            pure [Jmp addr]

        VLet a b c -> do
            -- TODO
            pure []

        _ ->
            error $ "unknown val: " ++ show val

data Param = PReg Reg
           | PImm Integer
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

        _ -> error $ show x


freshReg :: State GenState Reg
freshReg = do
    st <- get
    let num = getRegNum st
    put st { getRegNum = num + 1 }
    pure . Reg . pack $ "r" ++ show num