module TypeSystem.Common where

import Common.State
import Core.Types

import Data.ByteString.Char8 (ByteString, pack)
import Data.Char             (chr)

data GroupState s =
    GroupState { getVarNum      :: Int
               , getConstraints :: [(s, s)]
               } deriving Show

freshTVar :: State (GroupState s) (Type ByteString)
freshTVar = do
    gs <- get
    let n = getVarNum gs
    put gs { getVarNum = n + 1 }
    pure . TyVar $ numToVar n

numToVar :: Int -> ByteString
numToVar n =
    let (num, letter) = n `divMod` 26
    in pack (chr (letter + 97) : show num)
