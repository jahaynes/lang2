{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}

module Runtimes.MachineD ( runMachineD ) where

import Phase.CodeGen.TypesD

import Common.EitherT (EitherT, left, runEitherT)
import Common.ReaderT (ReaderT, ask, runReaderT)
import Common.StateT  (StateT, get, runStateT)
import Common.Trans   (lift)

import           Data.ByteString         (ByteString)
import           Data.Functor            ((<&>))
import           Data.String.Interpolate (i)
import           Data.Vector             ((!?), Vector)
import qualified Data.Vector as V

type D m a =
    StateT (MachineState ByteString) (
        ReaderT (MachineEnv ByteString) (
            EitherT ByteString IO)) a

runMachineD :: [DInstr ByteString] -> IO (Either ByteString ByteString)
runMachineD is =

    let env = MachineEnv { getInstrs = V.fromList is }

    in runEitherT (runReaderT initAndRun env) <&> \case
          Left s       -> Left s
          Right (x, _) -> Right x

initAndRun :: ReaderT (MachineEnv ByteString) (EitherT ByteString IO) (ByteString, MachineState ByteString)
initAndRun = do
    mainAt <- findMain
    let state = MachineState { getIp = mainAt }
    runStateT run state

findMain :: ReaderT (MachineEnv ByteString) (EitherT ByteString IO) Int
findMain = do

    is <- getInstrs <$> ask

    case V.findIndex isMain is of
            Just x  -> pure x
            Nothing -> lift $ left "No main found"

    where
    isMain (DLabel "main") = True
    isMain               _ = False

run :: D m ByteString
run = do

    instr <- currentInstr

    pure [i|#{instr}|]

currentInstr :: D m (DInstr ByteString)
currentInstr = do
    is <- getInstrs <$> lift ask
    ip <- getIp <$> get
    case is !? ip of
        Nothing -> lift . lift $ left "No such instruction!"
        Just x  -> pure x

data MachineEnv s =
    MachineEnv { getInstrs :: !(Vector (DInstr s)) }

data MachineState s =
    MachineState { getIp :: !Int }
