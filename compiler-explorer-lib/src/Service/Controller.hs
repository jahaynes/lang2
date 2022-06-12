{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.CallGraph
import Common.State
import Core.Module
import Parse.LexAndParse
import Parse.Token
import Pretty.Module
import TypeCheck.TypeCheckTypes
import TypeCheck.TypeInference

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Set                    (Set)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Vector                 (Vector)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lexAndParse" :> ReqBody '[PlainText] Text
                         :> Post '[JSON] ProgramState

data ProgramState =
    ProgramState { getSource        :: Text
                 , getTokens        :: Either ByteString (Vector Token)
                 , getModule        :: Either ByteString (Module ByteString)
                 , getCallGraph     :: Either ByteString (CallGraph ByteString)
                 , getTypeCheckPlan :: Either ByteString (TypeCheckPlan (Set ByteString)) -- TODO move this inside typechecker
                 , getInferred      :: Either ByteString (TypedModule ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens        = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns         = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns   = either decodeUtf8 render (getModule ps)
            txtCallGraph     = either decodeUtf8 (pack . show) (getCallGraph ps)
            txtTypeCheckPlan = either decodeUtf8 (pack . show) (getTypeCheckPlan ps)
            txtInferred      = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getInferred ps)

        object [ "tokens"        .= String txtTokens
               , "defns"         .= String txtDefns
               , "prettyDefns"   .= String txtPrettyDefns
               , "callGraph"     .= String txtCallGraph
               , "typeCheckPlan" .= String txtTypeCheckPlan
               , "inferred"      .= String txtInferred
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na na
    where
    na = Left "Not Available"

server :: Server Api
server = routeLexAndParse

    where
    routeLexAndParse :: Text -> Handler ProgramState
    routeLexAndParse = pure . transform

transform :: Text -> ProgramState
transform = execState pipe
          . fromSource

pipe :: State ProgramState ()
pipe = do
    phaseLexAndParse
    phaseTypeCheck
    --phaseEtaExpand
    --phaseSaturate
    --phaseContify
    --phaseOptimise
    --phaseClosureConvert
    --phaseLambdaLift

    where
    phaseLexAndParse :: State ProgramState ()
    phaseLexAndParse = modify' $ \ps ->
        let (eTokens, eMd) = lexAndParse . encodeUtf8 $ getSource ps
        in ps { getTokens = eTokens
              , getModule = eMd
              }

    phaseTypeCheck :: State ProgramState ()
    phaseTypeCheck = modify' $ \ps -> do

        let eModule       = getModule ps
            callGraph     = buildGraph <$> eModule
            typeCheckPlan = do
                cg <- callGraph
                md <- eModule
                planExcludingPretyped md cg

        let inferredModule = do
                md  <- eModule
                tcp <- TypeCheckPlan <$> typeCheckPlan
                inferModule md tcp

        ps { getCallGraph     = callGraph
           , getTypeCheckPlan = TypeCheckPlan <$> typeCheckPlan
           , getInferred      = inferredModule
           }

    --phaseEtaExpand :: State ProgramState ()
    --phaseEtaExpand = modify' $ \ps ->
        --ps { getEtaExpanded = etaExpand <$> getTypedModule ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
