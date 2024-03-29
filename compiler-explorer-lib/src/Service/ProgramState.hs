{-# LANGUAGE OverloadedStrings #-}

module Service.ProgramState where

import Core.Module
import Core.Types
import Parse.LexAndParse
import Parse.Token
import Phase.Anf.AnfModule
import Phase.CodeGen.CodeGenA
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
                 , getModule           :: !(Either ByteString (Module Untyped ByteString))
                 , getInferred         :: !(Either ByteString (Module (Type ByteString) ByteString))
                 , getEtaExpanded      :: !(Either ByteString (Module (Type ByteString) ByteString))
                 , getAnfConverted     :: !(Either ByteString (AnfModule ByteString))
                 , getClosureConverted :: !(Either ByteString (AnfModule ByteString))
                 , getLambdaLifted     :: !(Either ByteString (AnfModule ByteString))
                 , getUncurried        :: !(Either ByteString (AnfModule ByteString))
                 , getCodeGenA         :: !(Either ByteString [[AInstr ByteString]])
                 , getUnclobberedA     :: !(Either ByteString [[AInstr ByteString]])
                 , getOutput           :: !ByteString
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
            txtInferred               = either decodeUtf8 (\(Module _ _ tdefs) -> pack . unlines . map show $ tdefs) (getInferred ps)
            txtInferredPretty         = either decodeUtf8 renderTypedModule (getInferred ps)
            txtEtaExpanded            = either decodeUtf8 renderTypedModule (getEtaExpanded ps)
            txtAnfConverted           = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getAnfConverted ps)
            txtAnfPretty              = either decodeUtf8 renderAnfModule (getAnfConverted ps)
            txtClosureConverted       = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 renderAnfModule (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 renderAnfModule (getLambdaLifted ps)
            txtUncurried              = either decodeUtf8 (\(AnfModule _ anfdefs) -> pack . unlines . map show $ anfdefs) (getUncurried ps)
            txtUncurriedPretty        = either decodeUtf8 renderAnfModule (getUncurried ps)
            txtCodeGenA               = decodeUtf8 $ either id renderCodeGenA (concat <$> getCodeGenA ps)
            txtUnclobberedA           = decodeUtf8 $ either id renderCodeGenA (concat <$> getUnclobberedA ps)
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
               , "uncurried"              .= String txtUncurried
               , "uncurriedPretty"        .= String txtUncurriedPretty
               , "codeGenA"               .= String txtCodeGenA
               , "unclobberedA"           .= String txtUnclobberedA
               , "output"                 .= String txtOutput
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState (encodeUtf8 txt) na na na na na na na na na na na ""
    where
    na = Left "Not Available"
