{-# LANGUAGE OverloadedStrings #-}

module Pretty.TypedModule where

import Core.Expression
import Core.Module
import Core.Types
import Pretty.Operator
import Pretty.Term

import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

renderTypedModule :: TypedModule ByteString -> Text
renderTypedModule = TB.run . printTypedModule

printTypedModule :: TypedModule ByteString -> Builder
printTypedModule (TypedModule tFunDefns) = TB.intercalate "\n\n" (map printTFunDefn tFunDefns)

printTFunDefn :: TFunDefn ByteString -> Builder

printTFunDefn (TFunDefn n (ALam t vs body)) =

    let sig  = TB.intercalate " " [ bytestring n
                                  , ":"
                                  , printPolyType t ]

        impl = TB.intercalate " " [ bytestring n
                                  , printVars vs
                                  , "=\n" ]

    in TB.intercalate "\n" [sig, impl <> printTypedExpression 1 body]

printTFunDefn (TFunDefn n aexp) =

    let sig = TB.intercalate " " [ bytestring n
                                 , ":"
                                 , printPolyType (annot aexp) ]

        impl = TB.intercalate " " [ bytestring n
                                  , "=\n"
                                  , printTypedExpression 1 aexp ]

    in TB.intercalate "\n" [sig, impl]

printVars :: [ByteString] -> Builder
printVars = TB.intercalate " " . map bytestring

printPolyType :: Polytype ByteString -> Builder
printPolyType (Forall [] t) = printType t
printPolyType (Forall q  t) = mconcat ["forall ", printVars q, ". ", printType t]


--printType (TyCon c)   = bytestring c
--printType (TyVar v)   = bytestring v
--printType (TyArr a b) = mconcat ["(", printType a, " -> ", printType b, ")"]

printType :: Type ByteString -> Builder
printType = TB.intercalate " -> " . unbuild []
    where
    unbuild acc (TyArr a b) = unbuild (a:acc) b
    unbuild acc           t = reverse $ map prt (t:acc)

    prt (TyCon c) = bytestring c
    prt (TyVar v) = bytestring v
    prt t@TyArr{} = mconcat ["(", printType t,")"]

indent :: Int -> Builder
indent i = TB.text $ T.replicate (2*i) " "

printTypedExpression :: Int
                     -> AExpr (Polytype ByteString) ByteString
                     -> Builder

printTypedExpression ind aexp =

    case aexp of

        (ATerm (Forall _ t) term) ->
            TB.intercalate " " [ "(" <> printTerm (decodeUtf8 <$> term)
                               , ":"
                               , printType t <> ")"]

        (ALam _ vs body) ->
            let body' = printTypedExpression ind body
            in
            mconcat ["(\\", printVars vs, ". ", body', ")"]

        (AApp t f xs) ->
            let f'  = printTypedExpression ind f
                xs' = map (printTypedExpression ind) xs
            in mconcat ["(", TB.intercalate " " (f':xs'), " : ", printPolyType t, ")"]

        (ALet _ a b c) ->
            let a' = mconcat ["(", bytestring a, " : ", printPolyType (annot b), ")"]
            in TB.intercalate "\n" [ indent ind <> "let " <> a' <> " = " <> printTypedExpression ind b <> " in"
                                   , indent ind <> printTypedExpression ind c ]

        (AUnPrimOp _ op a) ->
            TB.intercalate " " [ printUnOp op
                               , printTypedExpression ind a ]
        (ABinPrimOp _ op a b) ->
            TB.intercalate " " [ printTypedExpression ind a
                               , printBinOp op
                               , printTypedExpression ind b ]

        (AIfThenElse _ pr tr fl) ->
            TB.intercalate "\n" [ indent  ind    <> "if "   <> printTypedExpression  ind    pr
                                , indent (ind+2) <> "then " <> printTypedExpression (ind+2) tr
                                , indent (ind+2) <> "else " <> printTypedExpression (ind+2) fl ]

        AClo{} ->
            error "not impl"

        ACallClo{} ->
            error "not impl"

bytestring :: ByteString -> Builder
bytestring = TB.text . decodeUtf8

-- query this while printing instead of returning
isCompound :: AExpr t s -> Bool
isCompound = undefined