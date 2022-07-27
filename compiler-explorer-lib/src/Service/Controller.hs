{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service.Controller (runController) where

import Common.CallGraph
import Common.State
import Core.Module
import Cps.CpsTyped
import Parse.LexAndParse
import Parse.Token
import Phase.ClosureConvertTyped
import Phase.EtaExpand
-- import Phase.Saturate
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
                 --, getSaturated        :: Either ByteString (TypedModule ByteString)
                 , getContified        :: Either ByteString (TypedModule ByteString)
                 , getClosureConverted :: Either ByteString (TypedModule ByteString)
                 }

instance ToJSON ProgramState where

    toJSON ps = do

        let txtTokens           = decodeUtf8 $ either id tokensToByteString (getTokens ps)
            txtDefns            = either decodeUtf8 moduleToText (getModule ps)
            txtPrettyDefns      = either decodeUtf8 render (getModule ps)
            txtInferred         = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getInferred ps)
            txtInferredPretty   = either decodeUtf8 renderTypedModule (getInferred ps)
            txtEtaExpanded      = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getEtaExpanded ps)
            txtEtaPretty        = either decodeUtf8 renderTypedModule (getEtaExpanded ps)
            --txtSaturated        = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getSaturated ps)
            --txtSaturatedPretty  = either decodeUtf8 renderTypedModule (getSaturated ps)
            txtContified        = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getContified ps)
            txtContifiedPretty  = either decodeUtf8 renderTypedModule (getContified ps)
            txtContifiedUntyped = either decodeUtf8 render (untypeModule <$> getContified ps)
            txtClosureConverted = either decodeUtf8 (\(TypedModule tdefs) -> pack . unlines . map show $ tdefs) (getClosureConverted ps)

        object [ "tokens"           .= String txtTokens
               , "defns"            .= String txtDefns
               , "prettyDefns"      .= String txtPrettyDefns
               , "inferred"         .= String txtInferred
               , "inferredPretty"   .= String txtInferredPretty
               , "etaExpanded"      .= String txtEtaExpanded
               , "etaPretty"        .= String txtEtaPretty
               --, "saturated"        .= String txtSaturated
               --, "saturatedPretty"  .= String txtSaturatedPretty 
               , "contified"        .= String txtContified
               , "contifiedPretty"  .= String txtContifiedPretty
               , "contifiedUntyped" .= String txtContifiedUntyped
               , "closureConverted" .= String txtClosureConverted
               ]

fromSource :: Text -> ProgramState
fromSource txt = ProgramState txt na na na na na na na na
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
    --phaseSaturate
    phaseContify
    --phaseOptimise
    --phaseClosureConvert
    --phaseLambdaLift

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

    --phaseSaturate :: State ProgramState ()
    --phaseSaturate = modify' $ \ps ->
        --ps { getSaturated = saturate <$> getEtaExpanded ps }

    phaseContify :: State ProgramState ()
    phaseContify = modify' $ \ps ->
        ps { getContified = cps <$> getEtaExpanded ps }

    phaseClosureConvert :: State ProgramState ()
    phaseClosureConvert = modify' $ \ps ->
        ps { getClosureConverted = closureConvert <$> getContified ps }

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
