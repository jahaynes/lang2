{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Parse.LexAndParse
import Pretty.Printer

import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lexAndParse" :> ReqBody '[PlainText] Text
                         :> Post '[JSON] (Text, Text, Text)

server :: Server Api
server = routeLexAndParse

    where
    routeLexAndParse :: Text -> Handler (Text, Text, Text)
    routeLexAndParse txt =

        let (eTokens, eDefns) =
                lexAndParse (encodeUtf8 txt)

            strTokens =
                either id tokensToByteString eTokens

            strDefns =
                either id definitionsToByteString eDefns

            strPretty =
                either decodeUtf8 render eDefns

        in
        pure ( decodeUtf8 strTokens
             , decodeUtf8 strDefns
             , strPretty
             )

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
