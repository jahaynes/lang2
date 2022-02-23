
{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer (runLexer) where

import Parse.Combinator  (many)
import Parse.LexState
import Parse.Parser      (Parser, (<|>), runParser)
import Parse.Primitives
import Parse.Token       (Token (..))

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import           Data.Char                   (isAlphaNum, isUpper)

runLexer :: ByteString -> Either String (LexState, [(SourcePos, Token ByteString)])
runLexer bs = runParser lexer (LexState (SourcePos 1 1) bs)

lexer :: Parser LexState [(SourcePos, Token ByteString)]
lexer = do
    skipSpaces
    toks <- many tok
    eof
    pure . filter (not . isComment) $ toks
    
    where
    isComment (_, t) = case t of
                         Comment _ -> True
                         _         -> False

    tok :: Parser LexState (SourcePos, Token ByteString)
    tok = withSourcePos $ do
            t <- lineComment <|> keywords <|> literals <|> punctuation <|> identifiers
            skipSpaces
            pure t

        where
        lineComment :: Parser LexState (Token ByteString)
        lineComment = string "--" *> (Comment <$> restOfLine)

        keywords :: Parser LexState (Token s)
        keywords = (TLet  <$ string "let"  <* notFollowedBy (\c -> c == '_' || isAlphaNum c)) --TODO tidy
               <|> (In    <$ string "in"   <* notFollowedBy (\c -> c == '_' || isAlphaNum c))
               <|> (If    <$ string "if"   <* notFollowedBy (\c -> c == '_' || isAlphaNum c))
               <|> (Then  <$ string "then" <* notFollowedBy (\c -> c == '_' || isAlphaNum c))
               <|> (Else  <$ string "else" <* notFollowedBy (\c -> c == '_' || isAlphaNum c))
               <|> (TErr  <$ string "err"  <* notFollowedBy (\c -> c == '_' || isAlphaNum c))
               <|> (TShow <$ string "show" <* notFollowedBy (\c -> c == '_' || isAlphaNum c))
               <|> (TData <$ string "data" <* notFollowedBy (\c -> c == '_' || isAlphaNum c))

        literals :: Parser LexState (Token ByteString)
        literals = litInt <|> litBool <|> litString
            where
            litInt    = TokInt    <$> int
            litBool   = TokBool   <$> bool
            litString = TokString <$> (char '\"' *> restOfString)
            --TODO escaping inside restOfString

        punctuation :: Parser LexState (Token s)
        punctuation = (LParen       <$ char   '(')
                  <|> (RParen       <$ char   ')')
                  <|> (DoubleEq     <$ string "==")
                  <|> (SingleEq     <$ char   '=')
                  <|> (Dot          <$ char   '.')
                  <|> (Dot          <$ string "->")
                  <|> (Lambda       <$ char   'λ')
                  <|> (Lambda       <$ char   '\\')
                  <|> (TStitch      <$ string "++")
                  <|> (Plus         <$ char   '+')
                  <|> (Minus        <$ char   '-')
                  <|> (Times        <$ char   '*')
                  <|> (Div          <$ char   '/')
                  <|> (Mod          <$ char   '%')

                  <|> (TLessEq      <$ string "<=")
                  <|> (TLessThan    <$ char   '<')
                  <|> (TGreaterEq   <$ string ">=")
                  <|> (TGreaterThan <$ char   '>')

                  <|> (TPipe        <$ char   '|')

        identifiers :: Parser LexState (Token ByteString)
        identifiers = do
            x  <- alpha <|> char '_'
            xs <- many (alphaNum <|> char '_')
            let bs = C8.pack (x:xs)
            pure $ if isUpper x
                     then UpperIdent bs
                     else LowerIdent bs
