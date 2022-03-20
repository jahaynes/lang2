{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Controller (runController) where

import Parse.LexAndParse

import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lexAndParse" :> ReqBody '[PlainText] Text
                         :> Post '[JSON] (Text, Text)

server :: Server Api
server = routeLexAndParse

    where
    routeLexAndParse :: Text -> Handler (Text, Text)
    routeLexAndParse txt =

        let (a, b) = lexAndParse (encodeUtf8 txt)
        in
        pure (decodeUtf8 a, decodeUtf8 b)

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
