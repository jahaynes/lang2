{-# LANGUAGE OverloadedStrings #-}

module Parse.Module where

import Core.Module
import Core.Types

import Core.Expression
import Parse.Expression
import Parse.Parser
import Parse.Token

import           Data.ByteString (ByteString)
import           Data.Vector      ((!?))
import qualified Data.IntSet as IS

data ModuleElement s = ModuleDataDefn (DataDefn s)
                     | ModuleTypeSig (TypeSig s)
                     | ModuleFunDefn (FunDefn s)

parseDefns :: Parser ParseState (Module ByteString)
parseDefns = go [] [] [] <$> many' parseDefn

    where
    go dds tss fds                      [] = Module (reverse dds) (reverse tss) (reverse fds)
    go dds tss fds (ModuleDataDefn dd:mes) = go (dd:dds) tss fds mes
    go dds tss fds (ModuleTypeSig ts:mes)  = go dds (ts:tss) fds mes
    go dds tss fds (ModuleFunDefn fd:mes)  = go dds tss (fd:fds) mes

parseDefn :: Parser ParseState (ModuleElement ByteString)
parseDefn = assertLineStart *> (ModuleDataDefn <$> parseDataDefn)
                           <|> (ModuleTypeSig  <$> parseTypeSig)
                           <|> (ModuleFunDefn  <$> parseFunDefn)

assertLineStart :: Parser ParseState ()
assertLineStart = Parser $ \ps ->
    case getColumn ps of
        Just 0 -> Right (ps, ())
        _      -> Left "Not a line start"

parseFunDefn :: Parser ParseState (FunDefn ByteString)
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

parseDataDefn :: Parser ParseState (DataDefn ByteString)
parseDataDefn = do
    name   <- atLineStart parseUpperStart
    tyVars <- notAtLineStarts parseLowerStart
    _      <- notAtLineStart (token TEq)
    dc1    <- notAtLineStart parseDataConstructor
    dcs    <- many' (notAtLineStart (token TPipe) *> notAtLineStart parseDataConstructor)
    pure $ DataDefn name tyVars (dc1:dcs)
    where
    parseDataConstructor :: Parser ParseState (DataCon ByteString)
    parseDataConstructor = DataCon <$> parseUpperStart
                                   <*> notAtLineStarts parseMember

    parseMember :: Parser ParseState (Member ByteString)
    parseMember = (token TLParen *> parseMember <* token TRParen)
              <|> parseMemberType
              <|> (MemberVar <$> parseLowerStart)

        where
        parseMemberType :: Parser ParseState (Member ByteString)
        parseMemberType = MemberType <$> notAtLineStart parseUpperStart
                                     <*> many' parseMember

parseTypeSig :: Parser ParseState (TypeSig ByteString)
parseTypeSig = do
    name <- atLineStart parseLowerStart
    _    <- notAtLineStart (token TColon)
    typ  <- parseArrowedType
    pure $ TypeSig name typ

    where
    parseArrowedType :: Parser ParseState (Type ByteString)
    parseArrowedType = do
        t  <- parseType
        ts <- many $ token TArr *> parseArrowedType
        pure $ foldl TyArr t ts

    parseType :: Parser ParseState (Type ByteString)
    parseType = (token TLParen *> parseArrowedType <* token TRParen)
            <|> (TyVar <$> parseLowerStart)
            <|> (TyCon <$> parseUpperStart <*> pure []) -- TODO unhardcode [] !
