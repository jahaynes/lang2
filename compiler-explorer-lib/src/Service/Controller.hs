{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Core.Module
import Phase.EtaExpand
import Parse.LexAndParse
import Parse.Token
import Pretty.Module
import Pretty.TypedModule
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
    ProgramState { getSource      :: Text
                 , getTokens      :: Either ByteString (Vector Token)
                 , getModule      :: Either ByteString (Module ByteString)
                 , getInferred    :: Either ByteString (ModuleT ByteString)
                 , getEtaExpanded :: Either ByteString (ModuleT ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens         = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns          = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns    = either decodeUtf8 render (getModule ps)
            txtInferred       = either decodeUtf8 (\(ModuleT tdefs) -> pack . unlines . map show $ tdefs) (getInferred ps)
            txtInferredPretty = either decodeUtf8 renderTypedModule (getInferred ps)
            txtEtaExpanded    = either decodeUtf8 renderTypedModule (getEtaExpanded ps)

        object [ "tokens"         .= String txtTokens
               , "defns"          .= String txtDefns
               , "prettyDefns"    .= String txtPrettyDefns
               , "inferred"       .= String txtInferred
               , "inferredPretty" .= String txtInferredPretty
               , "etaExpanded"    .= String txtEtaExpanded
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na
    where
    na = Left "Not Available"

server :: Server Api
server = pure . execState pipe . fromSource

pipe :: State ProgramState ()
pipe = do
    phaseLexAndParse
    phaseTypeCheck
    phaseEtaExpand

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

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
