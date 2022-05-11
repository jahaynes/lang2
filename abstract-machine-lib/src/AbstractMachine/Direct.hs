{-# LANGUAGE OverloadedStrings #-}

module AbstractMachine.Direct where

import AbstractMachine.Common
import Core.Definition
import Core.Expression

import           Data.ByteString (ByteString)
import qualified Data.Map as M

runDirect :: Module ByteString -> Either ByteString (Expr ByteString)
runDirect md = do

    let funDefs = M.fromList
                . map (\(FunDefn n e) -> (n, e))
                $ getFunDefns md

    case M.lookup "main" funDefs of
        Nothing   -> Left "No main!"
        Just main -> eval funDefs main
