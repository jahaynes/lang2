{-# LANGUAGE DataKinds,
             DeriveGeneric,
             TypeOperators #-}

module Service.Controller (runController) where

import Cps.Cps
import Optimise.Alpha
import Parse.LexAndParse
import Pretty.Printer
import TypeCheck.TypeCheck

import           Data.Aeson                  (ToJSON)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lexAndParse" :> ReqBody '[PlainText] Text
                         :> Post '[JSON] Response

data Response =
    Response { tokens          :: !Text
             , defns           :: !Text
             , prettyDefns     :: !Text
             , contified       :: !Text
             , prettyContified :: !Text
             , optimised       :: !Text
             , prettyOptimised :: !Text
             , types           :: !Text
             } deriving Generic

instance ToJSON Response

server :: Server Api
server = routeLexAndParse

    where
    routeLexAndParse :: Text -> Handler Response
    routeLexAndParse txt = do

        let (eTokens, eDefns) =
                lexAndParse (encodeUtf8 txt)

        pure $ Response { tokens          = decodeUtf8 $ either id tokensToByteString eTokens
                        , defns           = decodeUtf8 $ either id definitionsToByteString eDefns
                        , prettyDefns     = either decodeUtf8 render eDefns
                        , contified       = decodeUtf8 $ either id (definitionsToByteString . map cps) eDefns
                        , prettyContified = either decodeUtf8 (render . map cps) eDefns
                        , optimised       = decodeUtf8 $ either id (definitionsToByteString . alphas . map cps) eDefns
                        , prettyOptimised = either decodeUtf8 (render . alphas . map cps) eDefns
                        , types           = either decodeUtf8 (renderTypeEnv . runTypeCheck) eDefns
                        }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
