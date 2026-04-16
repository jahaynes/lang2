{-# LANGUAGE OverloadedStrings #-}

module Service.Service where

import Common.State
import Parse.LexAndParse
import Parse.Lexer
import Parse.Module
import Parse.Parser
import Phase.Anf.AnfModule
import Phase.CodeGen.CodeGenA
import Phase.CodeGen.CodeGenB
import Phase.ClosureConvert.ClosureConvert
import Phase.EtaExpand.EtaExpand
import Phase.LambdaLift.LambdaLift
import Phase.Unclobber.UnclobberRecursiveRegisters
import Phase.Uncurry.Uncurry
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
    phaseUncurry
    phaseCodeGenB
    -- phaseUnclobberRecursiveRegisters

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
        ps { getAnfConverted = anfModule =<< getEtaExpanded ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert =<< getAnfConverted ps }

    phaseLambdaLift :: State ProgramState ()
    phaseLambdaLift = modify' $ \ps ->
        ps { getLambdaLifted = lambdaLift <$> getClosureConverted ps }

    phaseUncurry :: State ProgramState ()
    phaseUncurry = modify' $ \ps ->
        ps { getUncurried = uncurryModule =<< getLambdaLifted ps } -- TODO

    phaseCodeGenB :: State ProgramState ()
    phaseCodeGenB = modify' $ \ps ->
        ps { getCodeGenA = codeGenModuleB =<< getLambdaLifted ps }

    phaseUnclobberRecursiveRegisters :: State ProgramState ()
    phaseUnclobberRecursiveRegisters = modify' $ \ps ->
        ps { getUnclobberedA = unclobberRecursiveRegisters =<< getCodeGenA ps }
