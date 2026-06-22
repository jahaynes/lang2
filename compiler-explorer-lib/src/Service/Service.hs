{-# LANGUAGE OverloadedStrings #-}

module Service.Service where

import Common.State
import Parse.LexAndParse
import Parse.Lexer
import Parse.Module
import Parse.Parser
import Phase.CodeGen.Direct
import Phase.EtaExpand.EtaExpand
import Service.ProgramState
import TypeSystem.TypeCheck

pipe :: State ProgramState ()
pipe = do
    phaseLexer
    phaseParser
    phaseTypeCheck
    phaseEtaExpand
    phaseCodeGen

    where
    phaseLexer :: State ProgramState ()
    phaseLexer = modify' $ \ps ->
        let positionsTokens = runLexer $ getSource ps in
        ps { getPositions = fst <$> positionsTokens
           , getTokens    = snd <$> positionsTokens }

    phaseParser :: State ProgramState ()
    phaseParser = modify' $ \ps ->
        let modul = do
                positions <- getPositions ps
                tokens    <- getTokens ps
                doParse parseDefns positions tokens $ findLineStarts (getSource ps) in
        ps { getModule = modul }

    phaseTypeCheck :: State ProgramState ()
    phaseTypeCheck = modify' $ \ps ->
        ps { getInferred = inferModule =<< getModule ps }

    phaseEtaExpand :: State ProgramState ()
    phaseEtaExpand = modify' $ \ps ->
        ps { getEtaExpanded = etaExpand <$> getInferred ps }

    phaseCodeGen :: State ProgramState ()
    phaseCodeGen = modify' $ \ps ->
        ps { getCodeGen = codeGen =<< getEtaExpanded ps }
