{-# LANGUAGE OverloadedStrings #-}

module AbstractMachine.Cps where

import AbstractMachine.Common
import Core.Definition
import Core.Expression
import Core.Term

import           Data.ByteString (ByteString)
import qualified Data.Map as M

runCps :: Module ByteString -> Either ByteString (Expr ByteString)
runCps md = do

    let funDefs = M.fromList
                . map (\(FunDefn n e) -> (n, e))
                $ getFunDefns md

    case M.lookup "main" funDefs of
        Nothing   -> Left "No main!"
        Just main -> eval funDefs (EApp main [ELam ["halt"] (ETerm (Var "halt"))])
