{-# LANGUAGE DataKinds,
             DeriveGeneric,
             LambdaCase,
             OverloadedStrings,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Runtimes.MachineA
import Service.ProgramState
import Service.Service                       (pipe)

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.Functor                ((<&>))
import           Data.IORef
import           Data.Text                   (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import           Servant
import           System.Directory            (listDirectory)
import           Text.Printf                 (printf)
import           UnliftIO.Exception          (tryAnyDeep)

newtype Input =
    Input { getInput :: Text
          } deriving (Generic, Show)

instance FromJSON Input

type Api = "lexAndParse" :> ReqBody '[JSON] Input
                         :> Post '[JSON] ProgramState

      :<|> "run" :> Post '[JSON] (Text, Text)

      :<|> "list-examples" :> Get '[JSON] [Text]

      :<|> "example" :> Capture "closure" Text :> Get '[JSON] Text

examplesDir :: FilePath
examplesDir = "./examples"

server :: IORef (Maybe ProgramState) -> Server Api
server ioref = setProgramState
          :<|> runCurrentProgramState
          :<|> listExamples
          :<|> getExample

    where
    setProgramState input = liftIO $ do
        writeIORef ioref Nothing
        let programState = execState pipe . fromSource $ getInput input
        writeIORef ioref (Just programState)
        pure programState

    runCurrentProgramState :: Handler (Text, Text)
    runCurrentProgramState = liftIO $
        readIORef ioref <&> \case
            Nothing -> ("No stored program!", "No stored program!")
            Just ps -> 
                case getUnclobberedA ps of
                    Left e1 -> ("", decodeUtf8 e1)
                    Right instrs ->
                        case runMachineA (concat instrs) of
                            (r1, r2) -> (decodeUtf8 r1, decodeUtf8 r2)

    listExamples :: Handler [Text]
    listExamples = liftIO $
        map T.pack <$> listDirectory examplesDir

    getExample :: Text -> Handler Text
    getExample name =
        let path = printf "%s/%s" examplesDir name
        in liftIO (tryAnyDeep (T.readFile path)) <&> \case
            Left e -> T.pack . show $ e
            Right t -> t

runController :: Int -> IO ()
runController port = do

    ioref <- newIORef Nothing

    run port . cors (const $ Just corsPolicy)
             . serve (Proxy :: Proxy Api)
             $ server ioref

corsPolicy :: CorsResourcePolicy
corsPolicy =
    CorsResourcePolicy { corsOrigins        = Just (["http://10.0.0.1:3000", "http://127.0.0.1:3000", "http://localhost:3000"], False)
                       , corsMethods        = []
                       , corsRequestHeaders = ["Content-Type"]
                       , corsExposedHeaders = Nothing
                       , corsMaxAge         = Nothing
                       , corsVaryOrigin     = False
                       , corsRequireOrigin  = True
                       , corsIgnoreFailures = False
                       }
