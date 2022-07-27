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
import Phase.ClosureConvert
import Phase.Cps
import Phase.DropTypes
import Phase.EtaExpand
import Phase.LambdaLift
import Pretty.Module
import Pretty.TypedModule
import TypeCheck.TypeCheckTypes
import TypeCheck.TypeInference

import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Set                    (Set)
import           Data.Text                   (Text, pack)
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
                 , getCallGraph        :: Either ByteString (CallGraph ByteString)
                 , getTypeCheckPlan    :: Either ByteString (TypeCheckPlan (Set ByteString)) -- TODO move this inside typechecker
                 , getInferred         :: Either ByteString (TypedModule ByteString)
                 , getEtaExpanded      :: Either ByteString (TypedModule ByteString)
                 , getUntyped          :: Either ByteString (Module ByteString)
                 , getContified        :: Either ByteString (Module ByteString)
                 , getClosureConverted :: Either ByteString (Module ByteString)
                 , getLambdaLifted     :: Either ByteString (Module ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens                 = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns                  = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns            = either decodeUtf8 render (getModule ps)
            txtInferred               = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getInferred ps)
            txtInferredPretty         = either decodeUtf8 renderTypedModule (getInferred ps)
            txtEtaExpanded            = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getEtaExpanded ps)
            txtEtaPretty              = either decodeUtf8 renderTypedModule (getEtaExpanded ps)
            txtContified              = either decodeUtf8 moduleToText (getContified ps)
            txtContifiedPretty        = either decodeUtf8 render (getContified ps)
            txtClosureConverted       = either decodeUtf8 moduleToText (getClosureConverted ps)
            txtClosureConvertedPretty = either decodeUtf8 render (getClosureConverted ps)
            txtLambdaLifted           = either decodeUtf8 moduleToText (getLambdaLifted ps)
            txtLambdaLiftedPretty     = either decodeUtf8 render (getLambdaLifted ps)

        object [ "tokens"                 .= String txtTokens
               , "defns"                  .= String txtDefns
               , "prettyDefns"            .= String txtPrettyDefns
               , "inferred"               .= String txtInferred
               , "inferredPretty"         .= String txtInferredPretty
               , "etaExpanded"            .= String txtEtaExpanded
               , "etaPretty"              .= String txtEtaPretty
               , "contified"              .= String txtContified
               , "contifiedPretty"        .= String txtContifiedPretty
               , "closureConverted"       .= String txtClosureConverted
               , "closureConvertedPretty" .= String txtClosureConvertedPretty
               , "lambdaLifted"           .= String txtLambdaLifted
               , "lambdaLiftedPretty"     .= String txtLambdaLiftedPretty
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na na na na na na na
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
    phaseLexAndParse
    phaseTypeCheck
    phaseEtaExpand
    phaseDropTypes
    phaseContify
    phaseClosureConvert
    phaseLambdaLift

    where
    phaseLexAndParse :: State ProgramState ()
    phaseLexAndParse = modify' $ \ps ->
        let (eTokens, eMd) = lexAndParse . encodeUtf8 $ getSource ps
        in ps { getTokens = eTokens
              , getModule = eMd
              }

    phaseTypeCheck :: State ProgramState ()
    phaseTypeCheck = modify' $ \ps -> do

        let eModule       = getModule ps
            callGraph     = buildGraph <$> eModule
            typeCheckPlan = do
                cg <- callGraph
                md <- eModule
                planExcludingPretyped md cg

        let inferredModule = do
                md  <- eModule
                tcp <- TypeCheckPlan <$> typeCheckPlan
                inferModule md tcp

        ps { getCallGraph     = callGraph
           , getTypeCheckPlan = TypeCheckPlan <$> typeCheckPlan
           , getInferred      = inferredModule
           }

    phaseEtaExpand :: State ProgramState ()
    phaseEtaExpand = modify' $ \ps ->
        ps { getEtaExpanded = etaExpand <$> getInferred ps }

    phaseDropTypes :: State ProgramState ()
    phaseDropTypes = modify' $ \ps ->
        ps { getUntyped = dropModuleTypes <$> getEtaExpanded ps }

    phaseContify :: State ProgramState ()
    phaseContify = modify' $ \ps ->
        ps { getContified = cps <$> getUntyped ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert <$> getContified ps }

    phaseLambdaLift :: State ProgramState ()
    phaseLambdaLift = modify' $ \ps ->
        ps { getLambdaLifted = lambdaLift <$> getClosureConverted ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
