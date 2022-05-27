{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.CallGraph
import Common.State
import Core.Module
import Parse.LexAndParse
import Parse.Token
import Phase.DropTypes
import Phase.EtaExpand
import Pretty.Module
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
import TypeCheck.TypeInference

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Set                    (Set)
import           Data.Text                   (Text, pack)
=======

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
>>>>>>> remove old types implementation
=======

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
>>>>>>> remove old types implementation
=======

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
>>>>>>> remove old types implementation
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Vector                 (Vector)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lexAndParse" :> ReqBody '[PlainText] Text
                         :> Post '[JSON] ProgramState

data ProgramState =
    ProgramState { getSource           :: Text
                 , getTokens           :: Either ByteString (Vector Token)
                 , getModule           :: Either ByteString (Module ByteString)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
                 , getCallGraph        :: Either ByteString (CallGraph ByteString)
                 , getTypeCheckPlan    :: Either ByteString [Set ByteString]
<<<<<<< HEAD
<<<<<<< HEAD
                 , getInferred         :: Either ByteString (PolytypeEnv ByteString)
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
                 , getTypedModule      :: Either ByteString (TypedModule ByteString)
                 , getEtaExpanded      :: Either ByteString (TypedModule ByteString)
<<<<<<< HEAD
>>>>>>> eta expansion
=======
>>>>>>> remove old types implementation
=======
                 , getTypedModule      :: Either ByteString (TypedModule ByteString)
                 , getEtaExpanded      :: Either ByteString (TypedModule ByteString)
>>>>>>> eta expansion
                 , getOptimised        :: Either ByteString (Module ByteString)
                 , getClosureConverted :: Either ByteString (Module ByteString)
                 , getLambdaLifted     :: Either ByteString (Module ByteString)
=======
                 , getDroppedTypes     :: Either ByteString [FunDefn ByteString]
>>>>>>> drop types
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
            txtCallGraph              = either decodeUtf8 (pack . show) (getCallGraph ps)
            txtTypeCheckPlan          = either decodeUtf8 (pack . show) (getTypeCheckPlan ps)
<<<<<<< HEAD
<<<<<<< HEAD
            txtInferred               = either decodeUtf8 (\(PolytypeEnv e) -> pack . unlines . map show $ M.toList e) (getInferred ps)
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
            txtTypedModule            = either decodeUtf8 (pack . unlines . map show . getTFunDefns) (getTypedModule ps)
            txtEtaExpanded            = either decodeUtf8 (pack . unlines . map show . getTFunDefns) (getEtaExpanded ps)
>>>>>>> eta expansion
=======
>>>>>>> remove old types implementation
=======
            txtTypedModule            = either decodeUtf8 (pack . unlines . map show . getTFunDefns) (getTypedModule ps)
            txtEtaExpanded            = either decodeUtf8 (pack . unlines . map show . getTFunDefns) (getEtaExpanded ps)
<<<<<<< HEAD
>>>>>>> eta expansion
            txtOptimised              = either decodeUtf8 moduleToText (getOptimised ps)
            txtPrettyOptimised        = either decodeUtf8 render (getOptimised ps)
            txtClosureConverted       = either decodeUtf8 moduleToText (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 render (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 moduleToText (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 render (getLambdaLifted ps)
=======
            txtDroppedTypes           = either decodeUtf8 (pack . unlines . map show) (getDroppedTypes ps)
>>>>>>> drop types

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
               , "callGraph"              .= String txtCallGraph
               , "typeCheckPlan"          .= String txtTypeCheckPlan
<<<<<<< HEAD
<<<<<<< HEAD
               , "inferred"               .= String txtInferred
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
               , "typedModule"            .= String txtTypedModule
               , "etaExpanded"            .= String txtEtaExpanded
<<<<<<< HEAD
>>>>>>> eta expansion
=======
>>>>>>> remove old types implementation
=======
               , "typedModule"            .= String txtTypedModule
               , "etaExpanded"            .= String txtEtaExpanded
>>>>>>> eta expansion
               , "optimised"              .= String txtOptimised
               , "prettyOptimised"        .= String txtPrettyOptimised
               , "closureConverted"       .= String txtClosureConverted
               , "closureConvertedPretty" .= String txtClosureConvertedPretty
               , "lambdaLifted"           .= String txtLambdaLifted
               , "lambdaLiftedPretty"     .= String txtLambdaLiftedPretty
               ]

fromSource :: Text -> ProgramState
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
fromSource txt = ProgramState txt na na na na na na na na
=======
fromSource txt = ProgramState txt na na na na na
>>>>>>> remove old types implementation
=======
fromSource txt = ProgramState txt na na na na na
>>>>>>> remove old types implementation
=======
fromSource txt = ProgramState txt na na na na na na na na na
>>>>>>> eta expansion
=======
fromSource txt = ProgramState txt na na na na na
>>>>>>> remove old types implementation
=======
fromSource txt = ProgramState txt na na na na na na na na na
>>>>>>> eta expansion
=======
               , "droppedTypes"           .= String txtDroppedTypes
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na na na na
>>>>>>> drop types
    where
    na = Left "Not Available"

server :: Server Api
server = routeLexAndParse

    where
    routeLexAndParse :: Text -> Handler ProgramState
    routeLexAndParse = pure . transform

transform :: Text -> ProgramState
transform = execState pipe
          . fromSource

pipe :: State ProgramState ()
pipe = do
    lexAndParser
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    phaseTypeCheck
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
    phaseEtaExpand
>>>>>>> eta expansion
=======
>>>>>>> remove old types implementation
=======
    phaseEtaExpand
<<<<<<< HEAD
>>>>>>> eta expansion
    optimise
    phaseClosureConvert
=======
    phaseDropTypes
>>>>>>> drop types

    where
    lexAndParser :: State ProgramState ()
    lexAndParser = modify' $ \ps ->
        let (eTokens, eMd) = lexAndParse . encodeUtf8 $ getSource ps
        in ps { getTokens = eTokens
              , getModule = eMd
              }

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    phaseTypeCheck :: State ProgramState ()
    phaseTypeCheck = modify' $ \ps -> do

        let eModule       = getModule ps
            callGraph     = buildGraph <$> eModule
            typeCheckPlan = do
                cg <- callGraph
                md <- eModule
                planExcludingPretyped md cg

        let typedModule = do
                md  <- eModule
                tcp <- typeCheckPlan
                inferModule md tcp

        ps { getCallGraph     = callGraph
           , getTypeCheckPlan = typeCheckPlan
           , getTypedModule   = typedModule
           }

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
=======
>>>>>>> eta expansion
    phaseEtaExpand :: State ProgramState ()
    phaseEtaExpand = modify' $ \ps ->
        ps { getEtaExpanded = etaExpand <$> getTypedModule ps }

<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> eta expansion
=======
>>>>>>> remove old types implementation
=======
>>>>>>> eta expansion
    optimise :: State ProgramState ()
    optimise = modify' $ \ps ->
        ps { getOptimised = alphas <$> getModule ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert <$> getOptimised ps }

    _phaseLambdaLift :: State ProgramState ()
    _phaseLambdaLift = modify' $ \ps ->
        ps { getLambdaLifted = lambdaLift <$> getClosureConverted ps }
=======
    phaseDropTypes :: State ProgramState ()
    phaseDropTypes = modify' $ \ps ->
        ps { getDroppedTypes = map dropTypes . getTFunDefns <$> getEtaExpanded ps }
>>>>>>> drop types

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
