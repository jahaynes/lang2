{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.CallGraph
import Common.State
import Core.Module
import Optimise.Alpha
import Parse.LexAndParse
import Parse.Token
import Phase.ClosureConvert
import Phase.LambdaLift
import Pretty.Module
import TypeCheck.TypeInference

import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.Map as M
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
    ProgramState { getSource           :: Text
                 , getTokens           :: Either ByteString (Vector Token)
                 , getModule           :: Either ByteString (Module ByteString)
                 , getCallGraph        :: Either ByteString (CallGraph ByteString)
                 , getTypeCheckPlan    :: Either ByteString [Set ByteString]
                 , getInferred         :: Either ByteString (PolytypeEnv ByteString)
                 , getOptimised        :: Either ByteString (Module ByteString)
                 , getClosureConverted :: Either ByteString (Module ByteString)
                 , getLambdaLifted     :: Either ByteString (Module ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
            txtCallGraph              = either decodeUtf8 (pack . show) (getCallGraph ps)
            txtTypeCheckPlan          = either decodeUtf8 (pack . show) (getTypeCheckPlan ps)
            txtInferred               = either decodeUtf8 (\(PolytypeEnv e) -> pack . unlines . map show $ M.toList e) (getInferred ps)
            txtOptimised              = either decodeUtf8 moduleToText (getOptimised ps)
            txtPrettyOptimised        = either decodeUtf8 render (getOptimised ps)
            txtClosureConverted       = either decodeUtf8 moduleToText (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 render (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 moduleToText (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 render (getLambdaLifted ps)

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
               , "callGraph"              .= String txtCallGraph
               , "typeCheckPlan"          .= String txtTypeCheckPlan
               , "inferred"               .= String txtInferred
               , "optimised"              .= String txtOptimised
               , "prettyOptimised"        .= String txtPrettyOptimised
               , "closureConverted"       .= String txtClosureConverted
               , "closureConvertedPretty" .= String txtClosureConvertedPretty
               , "lambdaLifted"           .= String txtLambdaLifted
               , "lambdaLiftedPretty"     .= String txtLambdaLiftedPretty
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na na na na na
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
    lexAndParser
    phaseTypeCheck
    optimise
    phaseClosureConvert
    phaseLambdaLift

    where
    lexAndParser :: State ProgramState ()
    lexAndParser = modify' $ \ps ->
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

        let inferred = do
                md  <- eModule
                tcp <- typeCheckPlan
                inferModule md tcp

        ps { getCallGraph     = callGraph
           , getTypeCheckPlan = typeCheckPlan
           , getInferred      = inferred
           }

    optimise :: State ProgramState ()
    optimise = modify' $ \ps ->
        ps { getOptimised = alphas <$> getModule ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert <$> getOptimised ps }

    phaseLambdaLift :: State ProgramState ()
    phaseLambdaLift = modify' $ \ps ->
        ps { getLambdaLifted = lambdaLift <$> getClosureConverted ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
