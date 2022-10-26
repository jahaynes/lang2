{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen2 where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.CodeGen1

import           Data.ByteString.Char8 (ByteString)
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
    GenState { regNum :: Int }

newtype Reg =
    Reg ByteString
        deriving Show

data Stmt = Pop ByteString -- reg?
          | Push ByteString -- reg?
          | Jmp ByteString
          | Cmp Reg Reg
          | JmpEq ByteString
              deriving Show

codeGenModule2 :: [(ByteString, Val)] -> IO ()
codeGenModule2 = mapM_ codeGen

    where
    codeGen (name, val) = do
        --print name
        --print val
        print $ emit val

emit :: Val -> [Stmt]
emit val =
    
    case val of

        VFun _si body ->
            -- Todo set up stack frame?
            emit body

        _ -> error $ "unknown val: " ++ show val