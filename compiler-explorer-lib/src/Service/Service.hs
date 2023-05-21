{-# LANGUAGE OverloadedStrings #-}

module Service.Service where

import Common.State
import Parse.LexAndParse
import Parse.Lexer
import Parse.Module
import Parse.Parser
import Phase.Anf.AnfModule
import Phase.CodeGen.CodeGen0
import Phase.CodeGen.CodeGen1
import Phase.ClosureConvert.ClosureConvert
import Phase.EtaExpand.EtaExpand
import Phase.LambdaLift.LambdaLift
import Service.ProgramState
import TypeSystem.TypeCheck

pipe :: State ProgramState ()
pipe = do
    phaseLexer
    phaseParser
    phaseTypeCheck
    phaseEtaExpand
    phaseAnfConvert
    phaseClosureConvert
    phaseLambdaLift
    phaseCodeGen0
    phaseCodeGen1

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

    phaseAnfConvert :: State ProgramState ()
    phaseAnfConvert = modify' $ \ps ->
        ps { getAnfConverted = anfModule <$> getEtaExpanded ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert <$> getAnfConverted ps }

    phaseLambdaLift :: State ProgramState ()
    phaseLambdaLift = modify' $ \ps ->
        ps { getLambdaLifted = lambdaLift <$> getClosureConverted ps }

    phaseCodeGen0 :: State ProgramState ()
    phaseCodeGen0 = modify' $ \ps ->
        ps { getCodeGen0 = codeGenModule0 <$> getLambdaLifted ps }

    phaseCodeGen1 :: State ProgramState ()
    phaseCodeGen1 = modify' $ \ps ->
        ps { getCodeGen1 = codeGenModule1 <$> getCodeGen0 ps }