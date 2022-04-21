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
import Phase.DiscardTypes
import Phase.EtaExpand
import Phase.Saturate
import Pretty.Printer
import TypeCheck.TypeCheck
import TypeCheck.TypedExpression
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
    ProgramState { getSource      :: Text
                 , getTokens      :: Either ByteString (Vector Token)
                 , getDefinitions :: Either ByteString [Defn ByteString]
                 , getTypeEnv     :: Either ByteString TypeEnv
                 , getTypedDefns  :: Either ByteString [TypedDefn Scheme ByteString]
                 , getEtaExpanded :: Either ByteString [TypedDefn Scheme ByteString]
                 , getSaturated   :: Either ByteString [TypedDefn Scheme ByteString]
                 , getDiscarded   :: Either ByteString [Defn ByteString]
                 , getContified   :: Either ByteString [Defn ByteString]
                 , getOptimised   :: Either ByteString [Defn ByteString]
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens          = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns           = decodeUtf8 $ either id definitionsToByteString (getDefinitions ps)
            txtPrettyDefns     = either decodeUtf8 render (getDefinitions ps)
            txtTypeEnv         = either decodeUtf8 renderTypeEnv (getTypeEnv ps) 
            txtTypes           = either decodeUtf8 renderTypedDefns (getTypedDefns ps)
            txtEtaExpanded     = either decodeUtf8 renderTypedDefns (getEtaExpanded ps)
            txtSaturated       = either decodeUtf8 renderTypedDefns (getSaturated ps)
            txtDiscarded       = either decodeUtf8 render (getDiscarded ps)
            txtContified       = decodeUtf8 $ either id definitionsToByteString (getContified ps)
            txtPrettyContified = either decodeUtf8 render (getContified ps)
            txtOptimised       = decodeUtf8 $ either id definitionsToByteString (getOptimised ps)
            txtPrettyOptimised = either decodeUtf8 render (getOptimised ps)

        object [ "tokens"          .= String txtTokens
               , "defns"           .= String txtDefns
               , "prettyDefns"     .= String txtPrettyDefns
               , "typeEnv"         .= String txtTypeEnv
               , "types"           .= String txtTypes
               , "etaExpanded"     .= String txtEtaExpanded
               , "discarded"       .= String txtDiscarded
               , "saturated"       .= String txtSaturated
               , "contified"       .= String txtContified
               , "prettyContified" .= String txtPrettyContified
               , "optimised"       .= String txtOptimised
               , "prettyOptimised" .= String txtPrettyOptimised
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
pipe = lexAndParser >> typeCheck >> phaseEtaExpand >> phaseSaturate >> phaseDiscardTypes >> phaseContify >> optimise

    where
    lexAndParser :: State ProgramState ()
    lexAndParser = modify' $ \ps ->
        let (eTokens, eDefns) = lexAndParse . encodeUtf8 $ getSource ps
        in ps { getTokens      = eTokens
              , getDefinitions = eDefns
              }

    typeCheck :: State ProgramState ()
    typeCheck = modify' $ \ps ->
        let (typedEnv, typedDefns) =
                case runTypeCheck <$> getDefinitions ps of
                    Left e       -> (Left e, Left e)
                    Right (a, b) -> (Right a, Right b)
        in ps { getTypeEnv    = typedEnv
              , getTypedDefns = typedDefns
              }

    phaseEtaExpand :: State ProgramState ()
    phaseEtaExpand = modify' $ \ps ->
        ps { getEtaExpanded = fmap etaExpand <$> getTypedDefns ps }

    phaseSaturate :: State ProgramState ()
    phaseSaturate = modify' $ \ps ->
        ps { getSaturated = fmap saturate <$> getEtaExpanded ps }

    phaseDiscardTypes :: State ProgramState ()
    phaseDiscardTypes = modify' $ \ps ->
        ps { getDiscarded = fmap discardTypes <$> getSaturated ps }

    phaseContify :: State ProgramState ()
    phaseContify = modify' $ \ps ->
        let preContified = map preCps <$> getDiscarded ps
        in ps { getContified = map cps <$> preContified }

    optimise :: State ProgramState ()
    optimise = modify' $ \ps ->
        ps { getOptimised = alphas <$> getContified ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
