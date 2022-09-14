{-# LANGUAGE OverloadedStrings #-}

module Pretty.AnfModule (renderAnfModule) where

import Core.Module
import Core.Types
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule
import Pretty.Operator
import Pretty.Term

import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

renderAnfModule :: AnfModule ByteString -> Text
renderAnfModule = TB.run . printAnfModule

printAnfModule :: AnfModule ByteString -> Builder
printAnfModule (AnfModule funDefnTs) = TB.intercalate "\n\n" (map printAnfFunDefn funDefnTs)


printAnfFunDefn :: FunDefAnfT ByteString -> Builder
printAnfFunDefn (FunDefAnfT n (Quant qs) expr) =

    case expr of

        AExp (ALam vs body) ->
            let impl = TB.intercalate " " [ bytestring n
                                          , printVars vs
                                          , "=\n" ]
            in TB.intercalate "\n" [impl <> printAnfExpression 1 body]

        _ ->
            let impl = TB.intercalate " " [ bytestring n
                                          , "=\n"
                                          , printAnfExpression 1 expr ]
            in TB.intercalate "\n" [bytestring n, impl]

   
-- TODO dedupe?
printPolyType :: Polytype ByteString -> Builder
printPolyType (Forall [] t) = printType t
printPolyType (Forall q  t) = mconcat ["forall ", printVars q, ". ", printType t]

-- TODO dedupe?
printVars :: [ByteString] -> Builder
printVars = TB.intercalate " " . map bytestring

-- TODO dedupe
printType :: Type ByteString -> Builder
printType = TB.intercalate " -> " . unbuild []
    where
    unbuild acc (TyArr a b) = unbuild (a:acc) b
    unbuild acc           t = reverse $ map prt (t:acc)

    prt (TyCon c) = bytestring c
    prt (TyVar v) = bytestring v
    prt t@TyArr{} = mconcat ["(", printType t,")"]

-- TODO dedupe
indent :: Int -> Builder
indent i = TB.text $ T.replicate (2*i) " "

printAnfExpression :: Int
                   -> NExp ByteString
                   -> Builder

printAnfExpression ind expr =

    case expr of

        AExp (ATerm term) ->
            printTerm (decodeUtf8 <$> term)

        AExp (ALam vs body) ->
            let body' = printAnfExpression ind body
            in
            mconcat ["(\\", printVars vs, ". ", body', ")"]

        AExp (AClo fvs vs body) ->
            let fvs'  = mconcat ["{", printVars fvs, "}"]
                vs'   = printVars vs
                body' = printAnfExpression ind body
            in mconcat ["(\\", fvs', " ", vs', ". ", body', ")"]

        CExp (CApp f xs) ->
            let f'  = printAnfExpression ind (AExp f)
                xs' = map (printAnfExpression ind . AExp) xs
            in TB.intercalate " " (f':xs')

        NLet a b c ->
            TB.intercalate "\n" [ indent ind <> "let " <> bytestring a <> " = " <> printAnfExpression ind b <> " in"
                                , indent ind <> printAnfExpression ind c ]
{-
        (AUnPrimOp _ op a) ->
            TB.intercalate " " [ printUnOp op
                               , printAnfExpression ind a ]
-}
        AExp (ABinPrimOp op a b) ->
            let x = TB.intercalate " " [ printAnfExpression ind (AExp a)
                                       , printBinOp op
                                       , printAnfExpression ind (AExp b) ]
            in mconcat ["(", x, ")"]

        CExp (CIfThenElse pr tr fl) ->
            TB.intercalate "\n" [ indent  ind    <> "if "   <> printAnfExpression  ind    (AExp pr)
                                , indent (ind+2) <> "then " <> printAnfExpression (ind+2) tr
                                , indent (ind+2) <> "else " <> printAnfExpression (ind+2) fl ]

-- TODO dedupe
bytestring :: ByteString -> Builder
bytestring = TB.text . decodeUtf8
