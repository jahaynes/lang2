{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Core.Module
import Parse.LexAndParse
import Parse.Token
import Phase.Anf.AnfModule
import Phase.CodeGen.CodeGen0
import Phase.CodeGen.CodeGen1
import Phase.ClosureConvert.ClosureConvert
import Phase.EtaExpand.EtaExpand
import Phase.LambdaLift.LambdaLift
import Pretty.Anf2
import Pretty.Module
import Pretty.TypedModule
import Runtimes.Machine1 (runMachine1)
import TypeSystem.TypeCheck

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Vector                 (Vector)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lexAndParse" :> ReqBody '[PlainText] Text
                         :> Post '[JSON] ProgramState

data ProgramState =
    ProgramState { getSource           :: Text
                 , getOutput           :: ByteString
                 , getTokens           :: Either ByteString (Vector Token)
                 , getModule           :: Either ByteString (Module ByteString)
                 , getInferred         :: Either ByteString (ModuleT ByteString)
                 , getEtaExpanded      :: Either ByteString (ModuleT ByteString)
                 , getAnfConverted     :: Either ByteString (AnfModule ByteString)
                 , getClosureConverted :: Either ByteString (AnfModule ByteString)
                 , getLambdaLifted     :: Either ByteString (AnfModule ByteString)
                 , getCodeGen0         :: Either ByteString [SubRoutine ByteString]
                 , getCodeGen1         :: Either ByteString [Instr ByteString]
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtOutput                 = decodeUtf8 $ getOutput ps
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
            txtCodeGen0               = either decodeUtf8 renderCodeGen0 (getCodeGen0 ps)
            txtCodeGen1               = either decodeUtf8 renderCodeGen1 (getCodeGen1 ps)

        object [ "output"                 .= String txtOutput
               , "tokens"                 .= String txtTokens
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
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt "" na na na na na na na na na
    where
    na = Left "Not Available"

server :: Server Api
server src = do
    let ps = execState pipe $ fromSource src
    case getCodeGen1 ps of
        Left e    -> pure ps { getOutput = e }
        Right cg1 -> pure ps { getOutput = runMachine1 cg1 }

pipe :: State ProgramState ()
pipe = do
    phaseLexAndParse
    phaseTypeCheck
    phaseEtaExpand
    phaseAnfConvert
    phaseClosureConvert
    phaseLambdaLift
    phaseCodeGen0
    phaseCodeGen1

    where
    phaseLexAndParse :: State ProgramState ()
    phaseLexAndParse = modify' $ \ps ->
        let (eTokens, eMd) = lexAndParse . encodeUtf8 $ getSource ps
        in ps { getTokens = eTokens
              , getModule = eMd
              }

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

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
