{-# LANGUAGE OverloadedStrings #-}

module Service.ProgramState where

import Core.Module
import Core.Types
import Parse.LexAndParse
import Parse.Token
import Phase.CodeGen.Direct
import Pretty.Direct
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
                 , getCodeGen          :: !(Either ByteString Code)
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
            txtCodeGen                = either decodeUtf8 renderCodeGen (getCodeGen ps)
            txtOutput                 = decodeUtf8 $ getOutput ps

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
               , "inferred"               .= String txtInferred
               , "inferredPretty"         .= String txtInferredPretty
               , "etaExpanded"            .= String txtEtaExpanded
               , "codeGen"                .= String txtCodeGen
               , "output"                 .= String txtOutput
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState (encodeUtf8 txt) na na na na na na ""
    where
    na = Left "Not Available"
