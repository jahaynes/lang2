{-# LANGUAGE OverloadedStrings #-}

module Parse2.Expression (parseExpr) where

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
    where

parseProduct :: Parser [Pos Token] (Pos (Expr ByteString))
parseProduct = fmap ETerm <$> parseTerm

parseTerm :: Parser [Pos Token] (Pos (Term ByteString))
parseTerm = parseLiteral

parseLiteral :: Parser [Pos Token] (Pos (Term ByteString))
parseLiteral = Parser f
    where
    f                      [] = Left "no more tokens for literal"
    f (Pos b (TLitBool x):ts) = Right (ts, Pos b (LitBool x))
    f (Pos b  (TLitInt i):ts) = Right (ts, Pos b (LitInt i))
    f                       _ = Left "Not a literal"

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

