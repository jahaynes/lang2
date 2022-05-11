{-# LANGUAGE DataKinds,
             OverloadedStrings,
             DeriveGeneric,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Core.Definition
import Optimise.Alpha
import Parse.LexAndParse
import Parse.Token
import Phase.ClosureConvert
import Phase.LambdaLift
import Pretty.Module

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
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
                 , getOptimised        :: Either ByteString (Module ByteString)
                 , getClosureConverted :: Either ByteString (Module ByteString)
                 , getLambdaLifted     :: Either ByteString (Module ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
            txtOptimised              = either decodeUtf8 moduleToText (getOptimised ps)
            txtPrettyOptimised        = either decodeUtf8 render (getOptimised ps)
            txtClosureConverted       = either decodeUtf8 moduleToText (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 render (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 moduleToText (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 render (getLambdaLifted ps)

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
               , "optimised"              .= String txtOptimised
               , "prettyOptimised"        .= String txtPrettyOptimised
               , "closureConverted"       .= String txtClosureConverted
               , "closureConvertedPretty" .= String txtClosureConvertedPretty
               , "lambdaLifted"           .= String txtLambdaLifted
               , "lambdaLiftedPretty"     .= String txtLambdaLiftedPretty
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
transform = snd 
          . runState pipe 
          . fromSource

pipe :: State ProgramState ()
pipe = do
    lexAndParser
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
