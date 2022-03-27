{-# LANGUAGE OverloadedStrings #-}

module Parse.Definition where

import Core.Definition

import Core.Expression
import Parse.Expression
import Parse.Parser
import Parse.Token

import Data.ByteString (ByteString)

import           Data.Vector      ((!?))
import qualified Data.IntSet as IS

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

atLineStart :: Parser ParseState a -> Parser ParseState a
atLineStart p = Parser $ \ps ->
    if isAtLineStart ps
        then runParser p ps
        else Left "Not at line start"

isAtLineStart :: ParseState -> Bool
isAtLineStart ps =
    case ps_positions ps !? ps_pos ps of
        Nothing -> False
        Just x  -> x `IS.member` ps_lineStarts ps

notAtLineStart :: Parser ParseState a -> Parser ParseState a
notAtLineStart p =
    Parser $ \ps ->
        if isAtLineStart ps
            then Left "At line start"
            else runParser p ps

notAtLineStarts :: Parser ParseState a -> Parser ParseState [a]
notAtLineStarts p = Parser $ go []
    where
    go acc ps   
        | isAtLineStart ps = Right (ps, reverse acc)
        | otherwise =
            case runParser p ps of
                Left _ -> Right (ps, reverse acc)
                Right (ps', x) -> go (x:acc) ps'

parseDataDefn :: Parser ParseState (Defn ByteString)
parseDataDefn = do
    name   <- atLineStart parseUpperStart
    tyVars <- notAtLineStarts parseTypeVariable
    _      <- notAtLineStart (token TEq)
    dc1    <- notAtLineStart parseDataConstructor
    dcs    <- many' (notAtLineStart (token TPipe) *> notAtLineStart parseDataConstructor)
    pure $ TypeDefn name tyVars (dc1:dcs)
    where
    parseDataConstructor :: Parser ParseState (DataCon ByteString)
    parseDataConstructor = DataCon <$> parseUpperStart <*> notAtLineStarts parseMember

    parseMember :: Parser ParseState (Member ByteString)
    parseMember = (MemberType <$> parseUpperStart)
              <|> (MemberVar  <$> parseLowerStart)

    parseTypeVariable :: Parser ParseState (TyVar ByteString)
    parseTypeVariable = TyVar <$> parseLowerStart
