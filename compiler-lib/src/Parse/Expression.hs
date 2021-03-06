{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Parse.Expression where

import Core.Expression
import Core.Operator
import Core.Term
import Parse.Parser
import Parse.Token

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.List             (foldl')
import           Data.Vector           ((!?))

parseExpr :: Parser ParseState (Expr ByteString)
parseExpr = parseBoolOr

parseBoolOr :: Parser ParseState (Expr ByteString)
parseBoolOr = do
    x  <- parseBoolAnd
    ys <- many $ do op <- boolOr
                    y  <- parseBoolAnd
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> EBinPrimOp o e y) x ys

    where
    boolOr :: Parser ParseState BinOp
    boolOr = parseSatisfy "boolOr" $ \case
                 TOr  -> Just OrB
                 _    -> Nothing

parseBoolAnd :: Parser ParseState (Expr ByteString)
parseBoolAnd = do
    x  <- parseComp
    ys <- many $ do op <- boolAnd
                    y  <- parseComp
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> EBinPrimOp o e y) x ys

    where
    boolAnd :: Parser ParseState BinOp
    boolAnd = parseSatisfy "boolAnd" $ \case
                  TAnd -> Just AndB
                  _    -> Nothing

parseComp :: Parser ParseState (Expr ByteString)
parseComp = sumExpr <|> parseSum
    where
    sumExpr = do
        x <- parseSum
        o <- compOp
        y <- parseComp
        pure $ EBinPrimOp o x y

    compOp :: Parser ParseState BinOp
    compOp = parseSatisfy "compOp" $ \case
                 TEqEq -> Just EqA
                 TGt   -> Just GtI
                 TGtEq -> Just GtEqI
                 TLt   -> Just LtI
                 TLtEq -> Just LtEqI
                 _     -> Nothing

parseSum :: Parser ParseState (Expr ByteString)
parseSum = do
    x  <- parseProduct
    ys <- many $ do op <- sumOp
                    y  <- parseProduct
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> EBinPrimOp o e y) x ys

    where
    sumOp :: Parser ParseState BinOp
    sumOp = parseSatisfy "sumOp" $ \case
                TPlus  -> Just AddI
                TMinus -> Just SubI
                _      -> Nothing

parseProduct :: Parser ParseState (Expr ByteString)
parseProduct = do
    x  <- parseApply
    ys <- many $ do op <- mulOp
                    y  <- parseApply
                    pure (op, y)
    pure $ foldl' (\e (o, y) -> EBinPrimOp o e y) x ys

    where
    mulOp :: Parser ParseState BinOp
    mulOp = parseSatisfy "mulOp" $ \case
                TMul -> Just MulI
                TDiv -> Just DivI
                _    -> Nothing

parseApply :: Parser ParseState (Expr ByteString)
parseApply = do
    (f, xs) <- parseWhileColumns1 MoreRight parseNonApply
    pure $ if null xs
               then f
               else EApp f xs

parseNonApply :: Parser ParseState (Expr ByteString)
parseNonApply = parseLet
            <|> parseIfThenElse
            <|> parseLambda
            <|> parseTerm
            <|> parseNegated
            <|> parseParen

parseNegated :: Parser ParseState (Expr ByteString)
parseNegated = do
    parseNegate
    EUnPrimOp Negate <$> parseExpr

parseLet :: Parser ParseState (Expr ByteString)
parseLet = do
    (f,xs) <- token TLet *> parseWhileColumns1 MoreRight parseLowerStart
    e1     <- token TEq  *> parseExpr
    e2     <- token TIn  *> parseExpr
    pure $ case xs of
        [] -> ELet f          e1  e2
        _  -> ELet f (ELam xs e1) e2

parseIfThenElse :: Parser ParseState (Expr ByteString)
parseIfThenElse = do
    p <- token TIf   *> parseExpr
    t <- token TThen *> parseExpr
    f <- token TElse *> parseExpr
    pure $ IfThenElse p t f

parseLambda :: Parser ParseState (Expr ByteString)
parseLambda = do
    (v, vs) <- token TLambda *> parseWhileColumns1 NotLeft parseLowerStart
    body    <- token TDot    *> parseExpr
    pure $ ELam (v:vs) body

parseParen :: Parser ParseState (Expr ByteString)
parseParen = token TLParen *> parseExpr <* token TRParen

parseTerm :: Parser ParseState (Expr ByteString)
parseTerm = parseLiteral <|> parseVariable

parseLiteral :: Parser ParseState (Expr ByteString)
parseLiteral = ETerm <$> parseLitString
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

parseVariable :: Parser ParseState (Expr ByteString)
parseVariable = pos <|> neg
    where
    pos = ETerm . Var <$> parseLowerStart
    neg = EUnPrimOp Negate . ETerm . Var <$> (parseNegate *> parseLowerStart)

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

token :: Token -> Parser ParseState ()
token tok =
    parseSatisfy (pack $ show tok) $ \t ->
        if t == tok
            then Just ()
            else Nothing
