{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Core.Definition
import Cps.Cps
import Optimise.Alpha
import Parse.LexAndParse
import Parse.Token
import Pretty.Printer
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
    ProgramState { getSource      :: Text
                 , getTokens      :: Either ByteString (Vector Token)
                 , getDefinitions :: Either ByteString [Defn ByteString]
                 , getContified   :: Either ByteString [Defn ByteString]
                 , getOptimised   :: Either ByteString [Defn ByteString]
                 , getTypes       :: Either ByteString TypeEnv
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens          = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns           = decodeUtf8 $ either id definitionsToByteString (getDefinitions ps)
            txtPrettyDefns     = either decodeUtf8 render (getDefinitions ps)
            txtContified       = decodeUtf8 $ either id definitionsToByteString (getContified ps)
            txtPrettyContified = either decodeUtf8 render (getContified ps)
            txtOptimised       = decodeUtf8 $ either id definitionsToByteString (getOptimised ps)
            txtPrettyOptimised = either decodeUtf8 render (getOptimised ps)
            txtTypes           = either decodeUtf8 renderTypeEnv (getTypes ps) 

        object [ "tokens"          .= String txtTokens
               , "defns"           .= String txtDefns
               , "prettyDefns"     .= String txtPrettyDefns
               , "contified"       .= String txtContified
               , "prettyContified" .= String txtPrettyContified
               , "optimised"       .= String txtOptimised
               , "prettyOptimised" .= String txtPrettyOptimised
               , "types"           .= String txtTypes
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
pipe = lexAndParser >> contify >> optimise >> typeCheck

    where
    lexAndParser :: State ProgramState ()
    lexAndParser = modify' $ \ps ->
        let (eTokens, eDefns) = lexAndParse . encodeUtf8 $ getSource ps
        in ps { getTokens      = eTokens
              , getDefinitions = eDefns
              }

    contify :: State ProgramState ()
    contify = modify' $ \ps ->
        ps { getContified = fmap cps <$> getDefinitions ps }

    optimise :: State ProgramState ()
    optimise = modify' $ \ps ->
        ps { getOptimised = alphas <$> getContified ps }

    typeCheck :: State ProgramState ()
    typeCheck = modify' $ \ps ->
        ps { getTypes = runTypeCheck <$> getDefinitions ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
