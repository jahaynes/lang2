{-# LANGUAGE DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Controller (runController) where

import Parse2.Expression
import Parse2.Lexer2
import Parse2.Parse2
import Parse2.Token2

import           Data.Text                   (Text)
import qualified Data.Text as T
import           Data.Text.Encoding          (encodeUtf8)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type Api = "lex" :> ReqBody '[PlainText] Text
                 :> Post '[PlainText] Text

      :<|> "parse" :> ReqBody '[PlainText] Text
                   :> Post '[PlainText] Text

server :: Server Api
server = routeLex :<|> routeParse

    where
    routeLex = pure . T.unlines
                    . map (T.pack . show)
                    . runLexer
                    . encodeUtf8

    routeParse strTokens = do

        let tokens :: [Pos Token] = map (read . T.unpack) 
                                  . T.lines
                                  $ strTokens

        pure . T.pack $
            case runParser parseExpr tokens of
                Left e -> show e
                Right (_, Pos _ e) -> show e

runController :: Int -> IO ()
runController port = run port . simpleCors $ serve (Proxy :: Proxy Api) server 
