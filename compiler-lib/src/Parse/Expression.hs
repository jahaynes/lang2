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

parseExpr :: Parser ParseState (Expr Untyped ByteString)
parseExpr = parseBoolOr

parseBoolOr :: Parser ParseState (Expr Untyped ByteString)
parseBoolOr = do
    x  <- parseBoolAnd
    ys <- many $ do op <- boolOr
                    y  <- parseBoolAnd
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOp Untyped o e y) x ys

    where
    boolOr :: Parser ParseState BinOp
    boolOr = parseSatisfy "boolOr" $ \case
                 TOr  -> Just OrB
                 _    -> Nothing

parseBoolAnd :: Parser ParseState (Expr Untyped ByteString)
parseBoolAnd = do
    x  <- parseComp
    ys <- many $ do op <- boolAnd
                    y  <- parseComp
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOp Untyped o e y) x ys

    where
    boolAnd :: Parser ParseState BinOp
    boolAnd = parseSatisfy "boolAnd" $ \case
                  TAnd -> Just AndB
                  _    -> Nothing

parseComp :: Parser ParseState (Expr Untyped ByteString)
parseComp = sumExpr <|> parseSum
    where
    sumExpr = do
        x <- parseSum
        o <- compOp
        y <- parseComp
        pure $ BinPrimOp Untyped o x y

    compOp :: Parser ParseState BinOp
    compOp = parseSatisfy "compOp" $ \case
                 TEqEq -> Just EqA
                 TGt   -> Just GtI
                 TGtEq -> Just GtEqI
                 TLt   -> Just LtI
                 TLtEq -> Just LtEqI
                 _     -> Nothing

parseSum :: Parser ParseState (Expr Untyped ByteString)
parseSum = do
    x  <- parseProduct
    ys <- many $ do op <- sumOp
                    y  <- parseProduct
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOp Untyped o e y) x ys

    where
    sumOp :: Parser ParseState BinOp
    sumOp = parseSatisfy "sumOp" $ \case
                TPlusPlus -> Just ConcatS
                TPlus     -> Just AddI
                TMinus    -> Just SubI
                _         -> Nothing

parseProduct :: Parser ParseState (Expr Untyped ByteString)
parseProduct = do
    x  <- parseApply
    ys <- many $ do op <- mulOp
                    y  <- parseApply
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> BinPrimOp Untyped o e y) x ys

    where
    mulOp :: Parser ParseState BinOp
    mulOp = parseSatisfy "mulOp" $ \case
                TMul -> Just MulI
                TDiv -> Just DivI
                _    -> Nothing

parseApply :: Parser ParseState (Expr Untyped ByteString)
parseApply = parseCase <|> parseApp
    where
    parseCase = do
        scrut    <- token TCase *> parseApply <* token TOf
        patterns <- parseWhileColumns NotLeft parsePattern
        pure $ Case Untyped scrut patterns

        where
        parsePattern :: Parser ParseState (Pattern Untyped ByteString)
        parsePattern = do
            a <- parseApp
            _ <- token TArr
            b <- parseExpr
            pure $ Pattern a b

    parseApp = do
        (f, xs) <- parseWhileColumns1 MoreRight parseNonApply
        pure $ if null xs
                then f
                else App Untyped f xs

parseNonApply :: Parser ParseState (Expr Untyped ByteString)
parseNonApply = parseLet
            <|> parseIfThenElse
            <|> parseLambda
            <|> parseTerm
            <|> parseNegated
            <|> parseShown
            <|> parseErr
            <|> parseParen

parseNegated :: Parser ParseState (Expr Untyped ByteString)
parseNegated = do
    parseNegate
    UnPrimOp Untyped Negate <$> parseExpr

parseShown :: Parser ParseState (Expr Untyped ByteString)
parseShown = do
    parseShow
    UnPrimOp Untyped EShow <$> parseExpr

parseErr :: Parser ParseState (Expr Untyped ByteString)
parseErr = do
    parseError
    UnPrimOp Untyped Err <$> parseExpr

parseLet :: Parser ParseState (Expr Untyped ByteString)
parseLet = do
    (f,xs) <- token TLet *> parseWhileColumns1 MoreRight parseLowerStart
    e1     <- token TEq  *> parseExpr
    e2     <- token TIn  *> parseExpr
    pure $ case xs of
        [] -> Let Untyped f                  e1  e2
        _  -> Let Untyped f (Lam Untyped xs e1) e2

parseIfThenElse :: Parser ParseState (Expr Untyped ByteString)
parseIfThenElse = do
    p <- token TIf   *> parseExpr
    t <- token TThen *> parseExpr
    f <- token TElse *> parseExpr
    pure $ IfThenElse Untyped p t f

parseLambda :: Parser ParseState (Expr Untyped ByteString)
parseLambda = do
    (v, vs) <- token TLambda *> parseWhileColumns1 NotLeft parseLowerStart
    body    <- token TDot    *> parseExpr
    pure $ Lam Untyped (v:vs) body

parseParen :: Parser ParseState (Expr Untyped ByteString)
parseParen = token TLParen *> parseExpr <* token TRParen

parseTerm :: Parser ParseState (Expr Untyped ByteString)
parseTerm = parseDataConstructor <|> parseLiteral <|> parseVariable

parseLiteral :: Parser ParseState (Expr Untyped ByteString)
parseLiteral = Term Untyped <$> parseLitString
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

parseVariable :: Parser ParseState (Expr Untyped ByteString)
parseVariable = pos <|> neg
    where
    pos = Term Untyped . Var <$> parseLowerStart
    neg = UnPrimOp Untyped Negate . Term Untyped . Var <$> (parseNegate *> parseLowerStart)

parseDataConstructor :: Parser ParseState (Expr Untyped ByteString)
parseDataConstructor = Term Untyped . DCons <$> parseUpperStart

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
