{-# LANGUAGE DataKinds,
             DeriveGeneric,
             LambdaCase,
             OverloadedStrings,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Runtimes.Machine2                     (runMachine2)
import Service.ProgramState
import Service.Service                       (pipe)

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.Functor                ((<&>))
import           Data.IORef
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import           Servant

newtype Input =
    Input { getInput :: Text
          } deriving (Generic, Show)

instance FromJSON Input

type Api = "lexAndParse" :> ReqBody '[JSON] Input
                         :> Post '[JSON] ProgramState

      :<|> "run" :> Post '[JSON] Text

      :<|> "example" :> Capture "closure" Text :> Get '[JSON] Text

server :: IORef (Maybe ProgramState) -> Server Api
server ioref = setProgramState :<|> runCurrentProgramState :<|> getExample

    where
    setProgramState input = liftIO $ do
        writeIORef ioref Nothing
        let programState = execState pipe . fromSource $ getInput input
        writeIORef ioref (Just programState)
        pure programState

    runCurrentProgramState =
        liftIO (readIORef ioref) >>= \case
            Nothing -> pure "<Not run>"
            Just ps ->
                case getCodeGen1 ps of
                    Left err -> pure "err"
                    Right ins ->
                        (liftIO $ runMachine2 ins) <&> \case
                            Left err  -> decodeUtf8 err
                            Right out -> decodeUtf8 out

    getExample "closure" =
        pure "f x =\n\
             \  let xx = x * x in\n\
             \  (\\y. y + xx)\n\
             \\n\
             \main = (f 1) 2"

runController :: Int -> IO ()
runController port = do

    ioref <- newIORef Nothing

    run port . cors (const $ Just corsPolicy)
             . serve (Proxy :: Proxy Api)
             $ server ioref

corsPolicy :: CorsResourcePolicy
corsPolicy =
    CorsResourcePolicy { corsOrigins        = Just (["http://127.0.0.1:3000", "http://localhost:3000"], False)
                       , corsMethods        = []
                       , corsRequestHeaders = ["Content-Type"]
                       , corsExposedHeaders = Nothing
                       , corsMaxAge         = Nothing
                       , corsVaryOrigin     = False
                       , corsRequireOrigin  = True
                       , corsIgnoreFailures = False
                       }
