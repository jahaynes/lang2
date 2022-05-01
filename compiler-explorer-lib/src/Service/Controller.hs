{-# LANGUAGE DataKinds,
             OverloadedStrings,
             DeriveGeneric,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Core.Definition
import Cps.Cps
import Cps.PreCps
import Optimise.Alpha
import Parse.LexAndParse
import Parse.Token
import Phase.ClosureConvert
import Phase.EtaExpand
import Phase.LambdaLift
import Phase.Saturate
import Pretty.Module
import TypeCheck.TypeCheck
import TypeCheck.Types

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
                 , getTypedModule      :: Either ByteString (TypedModule Scheme ByteString)
                 , getEtaExpanded      :: Either ByteString (TypedModule Scheme ByteString)
                 , getSaturated        :: Either ByteString (TypedModule Scheme ByteString)
                 , getContified        :: Either ByteString (Module ByteString)
                 , getOptimised        :: Either ByteString (Module ByteString)
                 , getClosureConverted :: Either ByteString (Module ByteString)
                 , getLambdaLifted     :: Either ByteString (Module ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
            txtEtaExpanded            = either decodeUtf8 typedModuleToText (getEtaExpanded ps)
            txtSaturated              = either decodeUtf8 typedModuleToText (getSaturated ps)
            txtContified              = either decodeUtf8 moduleToText (getContified ps)
            txtPrettyContified        = either decodeUtf8 render (getContified ps)
            txtOptimised              = either decodeUtf8 moduleToText (getOptimised ps)
            txtPrettyOptimised        = either decodeUtf8 render (getOptimised ps)
            txtClosureConverted       = either decodeUtf8 moduleToText (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 render (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 moduleToText (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 render (getLambdaLifted ps)

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
               , "etaExpanded"            .= String txtEtaExpanded
               , "saturated"              .= String txtSaturated
               , "contified"              .= String txtContified
               , "prettyContified"        .= String txtPrettyContified
               , "optimised"              .= String txtOptimised
               , "prettyOptimised"        .= String txtPrettyOptimised
               , "closureConverted"       .= String txtClosureConverted
               , "closureConvertedPretty" .= String txtClosureConvertedPretty
               , "lambdaLifted"           .= String txtLambdaLifted
               , "lambdaLiftedPretty"     .= String txtLambdaLiftedPretty
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na na na na na na
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
    typeCheck
    phaseEtaExpand
    phaseSaturate
    phaseContify
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

    typeCheck :: State ProgramState ()
    typeCheck = modify' $ \ps ->

        let md = getModule ps

            (_, typedDefns) =
                case runTypeCheck <$> getModule ps of
                    Left e       -> (Left e, Left e)
                    Right (a, b) -> (Right a, Right b)

            typedModule = TypedModule <$> (getDataDefns <$> md)
                                      <*> (getTypeSigs <$> md)
                                      <*> typedDefns

        in ps { getTypedModule = typedModule }

    phaseEtaExpand :: State ProgramState ()
    phaseEtaExpand = modify' $ \ps ->
        ps { getEtaExpanded = etaExpand <$> getTypedModule ps }

    phaseSaturate :: State ProgramState ()
    phaseSaturate = modify' $ \ps ->
        ps { getSaturated = saturate <$> getEtaExpanded ps }

    phaseContify :: State ProgramState ()
    phaseContify = modify' $ \ps ->
        let preContified = preCps <$> getSaturated ps
        in ps { getContified = cps <$> preContified }

    optimise :: State ProgramState ()
    optimise = modify' $ \ps ->
        ps { getOptimised = alphas <$> getContified ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert <$> getOptimised ps }

    phaseLambdaLift :: State ProgramState ()
    phaseLambdaLift = modify' $ \ps ->
        ps { getLambdaLifted = lambdaLift <$> getClosureConverted ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
