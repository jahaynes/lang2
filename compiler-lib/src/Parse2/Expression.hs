{-# LANGUAGE OverloadedStrings #-}

module Parse2.Expression (parseExpr, parseLowerStart) where

import Core.Expression
import Core.Operator
import Core.Term
import Parse2.Parse2
import Parse2.Token2

import Data.ByteString (ByteString)
import Data.List       (foldl')

parseExpr :: Parser [Pos Token] (Pos (Expr ByteString))
parseExpr = parseComp

parseComp :: Parser [Pos Token] (Pos (Expr ByteString))
parseComp = sumExpr <|> parseSum
    where
    sumExpr = do
        Pos b x <- parseSum
        Pos _ o <- compOp
        Pos _ y <- parseComp
        pure $ Pos b (EBinPrimOp o x y)

parseSum :: Parser [Pos Token] (Pos (Expr ByteString))
parseSum = do
    Pos b x <- parseProduct
    ys      <- many $ do Pos _ op <- sumOp
                         Pos _  y <- parseProduct
                         pure (op, y)
    pure $ Pos b (foldl' (\e (o, y) -> EBinPrimOp o e y) x ys)

parseProduct :: Parser [Pos Token] (Pos (Expr ByteString))
parseProduct = do
    Pos b x <- parseApply
    ys      <- many $ do Pos _ op <- mulOp
                         Pos _  y <- parseApply
                         pure (op, y)
    pure $ Pos b (foldl' (\e (o, y) -> EBinPrimOp o e y) x ys)

parseApply :: Parser [Pos Token] (Pos (Expr ByteString))
parseApply = do
    Pos b f <- parseNonApply
    xs      <- many $ do
                   Pos _ x <- parseNonApply
                   pure x
    pure $ Pos b $ if null xs
                       then f
                       else EApp f xs

parseNonApply :: Parser [Pos Token] (Pos (Expr ByteString))
parseNonApply = parseLet
            <|> parseIfThenElse
            <|> parseLambda
            <|> parseTerm
            <|> parseParen

parseLet :: Parser [Pos Token] (Pos (Expr ByteString))
parseLet = do
    Pos b _  <- satisfy (==TLet)
    negs     <- many1 parseLowerStart
    _        <- satisfy (==TEq)
    Pos _ e1 <- parseExpr
    _        <- satisfy (==TIn)
    Pos _ e2 <- parseExpr
    let (f:xs) = map (\(Pos _ v) -> v) negs
    pure $ Pos b $ case xs of
        [] -> ELet f          e1  e2
        _  -> ELet f (ELam xs e1) e2

parseIfThenElse :: Parser [Pos Token] (Pos (Expr ByteString))
parseIfThenElse = do
    Pos b _ <- satisfy (==TIf)
    Pos _ p <- parseExpr
    _       <- satisfy (==TThen)
    Pos _ t <- parseExpr
    _       <- satisfy (==TElse)
    Pos _ f <- parseExpr
    pure $ Pos b $ IfThenElse p t f

parseLambda :: Parser [Pos Token] (Pos (Expr ByteString))
parseLambda = do
    Pos b _    <- satisfy (==TLambda)
    params     <- fmap (\(Pos _ v) -> v) <$> many1 parseLowerStart
    _          <- satisfy (==TDot)
    Pos _ body <- parseExpr
    pure $ Pos b $ ELam params body

parseTerm :: Parser [Pos Token] (Pos (Expr ByteString))
parseTerm = parseLiteral <|> parseVariable

parseParen :: Parser [Pos Token] (Pos (Expr ByteString))
parseParen = satisfy (==TLParen) *> parseExpr <* satisfy (==TRParen)

parseLiteral :: Parser [Pos Token] (Pos (Expr ByteString))
parseLiteral = Parser f
    where
    f                                      [] = Left "no more tokens for literal"
    f (Pos b               (TLitBool x):ts)   = Right (ts, Pos b (ETerm (LitBool x)))
    f (Pos b               (TLitInt i):ts)    = Right (ts, Pos b (ETerm (LitInt i)))
    f (Pos b TNegate:Pos _ (TLitInt i):ts)    = Right (ts, Pos b (EUnPrimOp Negate (ETerm (LitInt i))))
    f (Pos b               (TLitString s):ts) = Right (ts, Pos b (ETerm (LitString s)))
    f                                       _ = Left "Not a literal"

parseVariable :: Parser [Pos Token] (Pos (Expr ByteString))
parseVariable = Parser f
    where
    f                                       [] = Left "no more tokens for variable"
    f (Pos b               (TLowerStart x):ts) = Right (ts, Pos b (ETerm (Var x)))
    f (Pos b TNegate:Pos _ (TLowerStart x):ts) = Right (ts, Pos b (EUnPrimOp Negate (ETerm (Var x))))
    f                                        _ = Left "Not a variable"

parseLowerStart :: Parser [Pos Token] (Pos ByteString)
parseLowerStart = Parser f
    where
    f                         [] = Left "no more tokens for parseLowerStart"
    f (Pos b (TLowerStart x):ts) = Right (ts, Pos b x)
    f                          _ = Left "Not a parseLowerStart" 

compOp :: Parser [Pos Token] (Pos BinOp)
compOp = Parser f
    where
    f               [] = Left "no more tokens for compOp"
    f (Pos b TEqEq:ts) = Right (ts, Pos b EqI)
    f (Pos b TGt:ts)   = Right (ts, Pos b GtI)
    f (Pos b TGtEq:ts) = Right (ts, Pos b GtEqI)
    f (Pos b TLt:ts)   = Right (ts, Pos b LtI)
    f (Pos b TLtEq:ts) = Right (ts, Pos b LtEqI)
    f                _ = Left "Not a comparator"

sumOp :: Parser [Pos Token] (Pos BinOp)
sumOp = Parser f
    where
    f                [] = Left "no more tokens for sumOp"
    f (Pos b TPlus:ts)  = Right (ts, Pos b AddI)
    f (Pos b TMinus:ts) = Right (ts, Pos b SubI)
    f                 _ = Left "Not summish"

mulOp :: Parser [Pos Token] (Pos BinOp)
mulOp = Parser f
    where
    f              [] = Left "no more tokens for mulOp"
    f (Pos b TMul:ts) = Right (ts, Pos b MulI)
    f (Pos b TDiv:ts) = Right (ts, Pos b DivI)
    f               _ = Left "Not mullish"

