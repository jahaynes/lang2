{-# LANGUAGE DataKinds,
             DeriveGeneric,
             LambdaCase,
             OverloadedStrings,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.State
import Runtimes.MachineC
import Service.ProgramState
import Service.Service                       (pipe)

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.IORef
import           Data.Text                   (Text, pack)
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

      :<|> "run" :> Post '[JSON] (Text, Text)

      :<|> "example" :> Capture "closure" Text :> Get '[JSON] Text

server :: IORef (Maybe ProgramState) -> Server Api
server ioref = setProgramState :<|> runCurrentProgramState :<|> getExample

    where
    setProgramState input = liftIO $ do
        writeIORef ioref Nothing
        let programState = execState pipe . fromSource $ getInput input
        writeIORef ioref (Just programState)
        pure programState

    runCurrentProgramState :: Handler (Text, Text) -- Why 2?
    runCurrentProgramState = liftIO $
        readIORef ioref >>= \case
            Nothing -> pure ("No stored program!", "No stored program!")
            Just ps -> 
                case getUnclobberedC ps of
                    Left e1 -> pure ("", decodeUtf8 e1)
                    Right instrs -> do
                        x <- interpret . concat $ instrs
                        let xt = pack (show x)
                        pure (xt, xt)
                        
                        
                        --case runMachineA (concat instrs) of
                        --    (r1, r2) -> (decodeUtf8 r1, decodeUtf8 r2)

    getExample "closure" =
        pure "f x =\n\
             \  let xx = x * x in\n\
             \  (\\y. y + xx)\n\
             \\n\
             \main = (f 1) 2"

    getExample "summorial" =
        pure "summorial m =\n\
             \   let go acc n =\n\
             \       if n == 0\n\
             \       then acc\n\
             \       else go (acc + n) (n - 1) in\n\
             \   go 0 m\n\
             \\n\
             \main = summorial 10"

    getExample "pair" =
        pure "Pair a b = MkPair a b\n\
             \\n\
             \main =\n\
             \  let snd pair =\n\
             \    case pair of\n\
             \      MkPair a b -> b\n\
             \  in\n\
             \  snd (MkPair 1 2)"

    getExample "reenter" =
        pure "reenter x =\n\
             \  if x == 0\n\
             \    then 0\n\
             \    else x + reenter (x - 1)\n\
             \\n\
             \main = reenter 5"

    getExample "badfibs" =
        pure  "badfibs n =\n\
              \  if n < 2\n\
              \    then n\n\
              \    else badfibs (n-1) + badfibs (n-2)\n\
              \\n\
              \main =\n\
              \  badfibs 4"

    getExample _ =
        pure "unknown example"

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
