{-# LANGUAGE OverloadedStrings #-}

module Service.ProgramState where

import Core.Module
import Parse.LexAndParse
import Parse.Token
import Phase.Anf.AnfModule
import Phase.CodeGen.CodeGen0
import Phase.CodeGen.CodeGen1
import Pretty.Anf2
import Pretty.Module

import Pretty.TypedModule

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Vector                 (Vector)

data ProgramState =
    ProgramState { getSource           :: !ByteString
                 , getPositions        :: !(Either ByteString (Vector Int))
                 , getTokens           :: !(Either ByteString (Vector Token))
                 , getModule           :: !(Either ByteString (Module ByteString))
                 , getInferred         :: !(Either ByteString (ModuleT ByteString))
                 , getEtaExpanded      :: !(Either ByteString (ModuleT ByteString))
                 , getAnfConverted     :: !(Either ByteString (AnfModule ByteString))
                 , getClosureConverted :: !(Either ByteString (AnfModule ByteString))
                 , getLambdaLifted     :: !(Either ByteString (AnfModule ByteString))
                 , getCodeGen0         :: !(Either ByteString [SubRoutine ByteString])
                 , getCodeGen1         :: !(Either ByteString [Instr ByteString])
                 , getOutput           :: !ByteString
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
            txtInferred               = either decodeUtf8 (\(ModuleT _ tdefs) -> pack . unlines . map show $ tdefs) (getInferred ps)
            txtInferredPretty         = either decodeUtf8 renderTypedModule (getInferred ps)
            txtEtaExpanded            = either decodeUtf8 renderTypedModule (getEtaExpanded ps)
            txtAnfConverted           = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getAnfConverted ps)
            txtAnfPretty              = either decodeUtf8 renderAnfModule (getAnfConverted ps)
            txtClosureConverted       = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 renderAnfModule (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 renderAnfModule (getLambdaLifted ps)
            txtCodeGen0               = decodeUtf8 $ either id renderCodeGen0 (getCodeGen0 ps)
            txtCodeGen1               = decodeUtf8 $ either id renderCodeGen1 (getCodeGen1 ps)
            txtOutput                 = decodeUtf8 $ getOutput ps

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
               , "inferred"               .= String txtInferred
               , "inferredPretty"         .= String txtInferredPretty
               , "etaExpanded"            .= String txtEtaExpanded
               , "anfConverted"           .= String txtAnfConverted
               , "anfPretty"              .= String txtAnfPretty
               , "closureConverted"       .= String txtClosureConverted
               , "closureConvertedPretty" .= String txtClosureConvertedPretty
               , "lambdaLifted"           .= String txtLambdaLifted
               , "lambdaLiftedPretty"     .= String txtLambdaLiftedPretty
               , "codeGen0"               .= String txtCodeGen0
               , "codeGen1"               .= String txtCodeGen1
               , "output"                 .= String txtOutput
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState (encodeUtf8 txt) na na na na na na na na na na ""
    where
    na = Left "Not Available"
