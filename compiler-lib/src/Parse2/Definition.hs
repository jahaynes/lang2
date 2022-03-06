module Parse2.Definition where

import Core.Definition

import Core.Expression
import Core.Term
import Parse2.Expression
import Parse2.Parse2
import Parse2.Token2

import Data.ByteString (ByteString)

parseDefns :: Parser [Pos Token] [Pos (Defn ByteString)]
parseDefns = (:) <$> parseDefn
                 <*> many (satisfy (==TBreak) *> parseDefn)

parseDefn :: Parser [Pos Token] (Pos (Defn ByteString))
parseDefn = do
    Pos b name <- fmap (\(Var v) -> v) <$> parseVariable
    vars       <- many ((\(Pos _ (Var v)) -> v) <$> parseVariable)
    _          <- satisfy (==TEq)
    Pos _ expr <- parseExpr

    pure $ Pos b $
        case vars of
            [] -> Defn name expr
            _  -> Defn name (ELam vars expr)