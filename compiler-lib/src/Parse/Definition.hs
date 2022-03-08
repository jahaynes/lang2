{-# LANGUAGE OverloadedStrings #-}

module Parse.Definition where

import Core.Definition

import Core.Expression
import Parse.Expression
import Parse.Parser
import Parse.Token

import Data.ByteString (ByteString)

parseDefns :: Parser [Pos Token] [Pos (Defn ByteString)]
parseDefns = (:) <$> parseDefn
                 <*> many (satisfy (==TBreak) *> parseDefn)

parseDefn :: Parser [Pos Token] (Pos (Defn ByteString))
parseDefn = parseFunDefn <|> parseDataDefn

parseFunDefn :: Parser [Pos Token] (Pos (Defn ByteString))
parseFunDefn = do
    Pos b name <- parseLowerStart
    vars       <- many ((\(Pos _ v) -> v) <$> parseLowerStart)
    _          <- satisfy (==TEq)
    Pos _ expr <- parseExpr
    pure $ Pos b $ FunDefn name $
        case vars of
            [] -> expr
            _  -> ELam vars expr

parseDataDefn :: Parser [Pos Token] (Pos (Defn ByteString))
parseDataDefn = do
    Pos b name   <- parseUpperStart
    tyVars       <- many ((\(Pos _ v) -> v) <$> parseTypeVariable)
    _            <- satisfy (==TEq)
    constructors <- (:) <$> parseConstructor
                        <*> many (many (satisfy (==TBreak)) *> satisfy (==TPipe) *> parseConstructor)
    pure $ Pos b $ TypeDefn name tyVars constructors

parseConstructor :: Parser [Pos Token] (DataCon ByteString)
parseConstructor = do
    Pos _ n <- parseUpperStart
    ms      <- many ((\(Pos _ v) -> v) <$> parseMember)
    pure $ DataCon n ms

parseMember :: Parser [Pos Token] (Pos (Member ByteString))
parseMember = (fmap MemberType <$> parseUpperStart)
          <|> (fmap MemberVar  <$> parseLowerStart)

parseTypeVariable :: Parser [Pos Token] (Pos (TyVar ByteString))
parseTypeVariable = fmap TyVar <$> parseLowerStart

parseUpperStart :: Parser [Pos Token] (Pos ByteString)
parseUpperStart = Parser f
    where
    f                         [] = Left "no more tokens for parseUpperStart"
    f (Pos b (TUpperStart x):ts) = Right (ts, Pos b x)
    f                          _ = Left "Not a parseUpperStart"
