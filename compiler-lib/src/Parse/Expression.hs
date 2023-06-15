{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Parse.Expression where

import Core.Expression
import Core.Operator
import Core.Term
import Core.Types (Untyped (..))
import Parse.Parser
import Parse.Token

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.List             (foldl')
import           Data.Vector           ((!?))

parseExpr :: Parser ParseState (ExprT Untyped ByteString)
parseExpr = parseBoolOr

parseBoolOr :: Parser ParseState (ExprT Untyped ByteString)
parseBoolOr = do
    x  <- parseBoolAnd
    ys <- many $ do op <- boolOr
                    y  <- parseBoolAnd
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOpT Untyped o e y) x ys

    where
    boolOr :: Parser ParseState BinOp
    boolOr = parseSatisfy "boolOr" $ \case
                 TOr  -> Just OrB
                 _    -> Nothing

parseBoolAnd :: Parser ParseState (ExprT Untyped ByteString)
parseBoolAnd = do
    x  <- parseComp
    ys <- many $ do op <- boolAnd
                    y  <- parseComp
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOpT Untyped o e y) x ys

    where
    boolAnd :: Parser ParseState BinOp
    boolAnd = parseSatisfy "boolAnd" $ \case
                  TAnd -> Just AndB
                  _    -> Nothing

parseComp :: Parser ParseState (ExprT Untyped ByteString)
parseComp = sumExpr <|> parseSum
    where
    sumExpr = do
        x <- parseSum
        o <- compOp
        y <- parseComp
        pure $ BinPrimOpT Untyped o x y

    compOp :: Parser ParseState BinOp
    compOp = parseSatisfy "compOp" $ \case
                 TEqEq -> Just EqA
                 TGt   -> Just GtI
                 TGtEq -> Just GtEqI
                 TLt   -> Just LtI
                 TLtEq -> Just LtEqI
                 _     -> Nothing

parseSum :: Parser ParseState (ExprT Untyped ByteString)
parseSum = do
    x  <- parseProduct
    ys <- many $ do op <- sumOp
                    y  <- parseProduct
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOpT Untyped o e y) x ys

    where
    sumOp :: Parser ParseState BinOp
    sumOp = parseSatisfy "sumOp" $ \case
                TPlusPlus -> Just ConcatS
                TPlus     -> Just AddI
                TMinus    -> Just SubI
                _         -> Nothing

parseProduct :: Parser ParseState (ExprT Untyped ByteString)
parseProduct = do
    x  <- parseApply
    ys <- many $ do op <- mulOp
                    y  <- parseApply
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOpT Untyped o e y) x ys

    where
    mulOp :: Parser ParseState BinOp
    mulOp = parseSatisfy "mulOp" $ \case
                TMul -> Just MulI
                TDiv -> Just DivI
                _    -> Nothing

parseApply :: Parser ParseState (ExprT Untyped ByteString)
parseApply = parseCase <|> parseApp
    where
    parseCase = do
        scrut    <- token TCase *> parseApply <* token TOf
        patterns <- parseWhileColumns NotLeft parsePattern
        pure $ CaseT Untyped scrut patterns

        where
        parsePattern :: Parser ParseState (PatternT Untyped ByteString)
        parsePattern = do
            a <- parseApp
            _ <- token TArr
            b <- parseExpr
            pure $ PatternT a b

    parseApp = do
        (f, xs) <- parseWhileColumns1 MoreRight parseNonApply
        pure $ if null xs
                then f
                else AppT Untyped f xs

parseNonApply :: Parser ParseState (ExprT Untyped ByteString)
parseNonApply = parseLet
            <|> parseIfThenElse
            <|> parseLambda
            <|> parseTerm
            <|> parseNegated
            <|> parseShown
            <|> parseErr
            <|> parseParen

parseNegated :: Parser ParseState (ExprT Untyped ByteString)
parseNegated = do
    parseNegate
    UnPrimOpT Untyped Negate <$> parseExpr

parseShown :: Parser ParseState (ExprT Untyped ByteString)
parseShown = do
    parseShow
    UnPrimOpT Untyped EShow <$> parseExpr

parseErr :: Parser ParseState (ExprT Untyped ByteString)
parseErr = do
    parseError
    UnPrimOpT Untyped Err <$> parseExpr

parseLet :: Parser ParseState (ExprT Untyped ByteString)
parseLet = do
    (f,xs) <- token TLet *> parseWhileColumns1 MoreRight parseLowerStart
    e1     <- token TEq  *> parseExpr
    e2     <- token TIn  *> parseExpr
    pure $ case xs of
        [] -> LetT Untyped f                  e1  e2
        _  -> LetT Untyped f (LamT Untyped xs e1) e2

parseIfThenElse :: Parser ParseState (ExprT Untyped ByteString)
parseIfThenElse = do
    p <- token TIf   *> parseExpr
    t <- token TThen *> parseExpr
    f <- token TElse *> parseExpr
    pure $ IfThenElseT Untyped p t f

parseLambda :: Parser ParseState (ExprT Untyped ByteString)
parseLambda = do
    (v, vs) <- token TLambda *> parseWhileColumns1 NotLeft parseLowerStart
    body    <- token TDot    *> parseExpr
    pure $ LamT Untyped (v:vs) body

parseParen :: Parser ParseState (ExprT Untyped ByteString)
parseParen = token TLParen *> parseExpr <* token TRParen

parseTerm :: Parser ParseState (ExprT Untyped ByteString)
parseTerm = parseDataConstructor <|> parseLiteral <|> parseVariable

parseLiteral :: Parser ParseState (ExprT Untyped ByteString)
parseLiteral = TermT Untyped <$> parseLitString
                             <|> parseLitBool
                             <|> parseLitInt

parseLitString :: Parser ParseState (Term ByteString)
parseLitString = parseSatisfy "string" f
    where
    f (TLitString s) = Just (LitString s)
    f _              = Nothing

parseLitBool :: Parser ParseState (Term ByteString)
parseLitBool = parseSatisfy "boolean" f
    where
    f (TLitBool b) = Just (LitBool b)
    f _            = Nothing

parseLitInt :: Parser ParseState (Term ByteString)
parseLitInt = pos <|> neg

    where
    pos = parseSatisfy "integer" isInt
    neg = do
        parseNegate
        ji <- parseSatisfy "integer" isInt
        case ji of
            LitInt i -> pure $ LitInt (-i)
            _        -> error "isInt returned non-int"
            
    isInt (TLitInt i) = Just (LitInt i)
    isInt           _ = Nothing

parseVariable :: Parser ParseState (ExprT Untyped ByteString)
parseVariable = pos <|> neg
    where
    pos = TermT Untyped . Var <$> parseLowerStart
    neg = UnPrimOpT Untyped Negate . TermT Untyped . Var <$> (parseNegate *> parseLowerStart)

parseDataConstructor :: Parser ParseState (ExprT Untyped ByteString)
parseDataConstructor = TermT Untyped . DCons <$> parseUpperStart

parseSatisfy :: ByteString
             -> (Token -> Maybe a)
             -> Parser ParseState a
parseSatisfy n p = Parser f
    where
    f ps@(ParseState tokens pos _ _)

        | pos == length tokens =
            Left $ "no more tokens for " <> n

        | otherwise =
            case tokens !? pos of
                Nothing -> Left "Failed index"
                Just t  ->
                    case p t of
                        Nothing -> Left $ "Not a " <> n
                        Just r  -> Right (ps {ps_pos = pos + 1}, r)

parseLowerStart :: Parser ParseState ByteString
parseLowerStart = parseSatisfy "lowerStart" f
    where
    f (TLowerStart x) = Just x
    f               _ = Nothing

parseUpperStart :: Parser ParseState ByteString
parseUpperStart = parseSatisfy "upperStart" f
    where
    f (TUpperStart x) = Just x
    f               _ = Nothing

parseNegate :: Parser ParseState ()
parseNegate = parseSatisfy "negate" f
    where
    f TNegate = Just ()
    f       _ = Nothing

parseShow :: Parser ParseState ()
parseShow = parseSatisfy "show" f
    where
    f TDollar = Just ()
    f       _ = Nothing

parseError :: Parser ParseState ()
parseError = parseSatisfy "error" f
    where
    f TErr = Just ()
    f    _ = Nothing

token :: Token -> Parser ParseState ()
token tok =
    parseSatisfy (pack $ show tok) $ \t ->
        if t == tok
            then Just ()
            else Nothing
