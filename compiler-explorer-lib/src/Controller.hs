{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controller (runController) where

import Control.Monad.IO.Class   (liftIO)
import Data.Text                (Text)
import Network.Wai.Handler.Warp (run)
import Servant

type Api = "foo" :> Get '[JSON] Text

server :: Server Api
server = do
    liftIO $ putStrLn "Hi route"
    pure "Hi"

runController :: Int -> IO ()
runController port = run port $ serve (Proxy :: Proxy Api) server 
