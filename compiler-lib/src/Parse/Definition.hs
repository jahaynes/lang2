{-# LANGUAGE OverloadedStrings #-}

module Parse.Definition where

import Core.Definition

import Core.Expression
import Parse.Expression
import Parse.Parser
import Parse.Token

import Data.ByteString (ByteString)

parseDefns :: Parser ParseState [Defn ByteString]
parseDefns = many' parseDefn

parseDefn :: Parser ParseState (Defn ByteString)
parseDefn = assertLineStart *> (parseFunDefn <|> parseDataDefn)

assertLineStart :: Parser ParseState ()
assertLineStart = Parser $ \ps ->
    case getColumn ps of
        Just 0 -> Right (ps, ())
        _      -> Left "Not a line start"

parseFunDefn :: Parser ParseState (Defn ByteString)
parseFunDefn = do
    (name, vars) <- parseWhileColumns1 MoreRight parseLowerStart
    token TEq
    expr <- parseExpr
    pure $ FunDefn name $
        case vars of
            [] -> expr
            _  -> ELam vars expr

parseDataDefn :: Parser ParseState (Defn ByteString)
parseDataDefn = do
    name         <- parseUpperStart
    tyVars       <- parseWhileColumns MoreRight parseTypeVariable
    _            <- token TEq
    constructors <- (:) <$> parseConstructor
                        <*> many' (token TPipe *> parseConstructor)
    pure $ TypeDefn name tyVars constructors

parseConstructor :: Parser ParseState (DataCon ByteString)
parseConstructor = DataCon <$> parseUpperStart <*> parseWhileColumns NotLeft parseMember

parseMember :: Parser ParseState (Member ByteString)
parseMember = (MemberType <$> parseUpperStart)
          <|> (MemberVar  <$> parseLowerStart)

parseTypeVariable :: Parser ParseState (TyVar ByteString)
parseTypeVariable = TyVar <$> parseLowerStart
