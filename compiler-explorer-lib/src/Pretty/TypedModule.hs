{-# LANGUAGE OverloadedStrings #-}

module Pretty.TypedModule (renderTypedModule) where

import Core.Expression
import Core.Module
import Core.Types
import Pretty.Common
import Pretty.Operator
import Pretty.Term
import Pretty.Type

import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import           Data.Text.Encoding
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

renderTypedModule :: ModuleT (Type ByteString) ByteString -> Text
renderTypedModule = TB.run . printTypedModule

printTypedModule :: ModuleT (Type ByteString) ByteString -> Builder
printTypedModule (ModuleT _ funDefnTs) = TB.intercalate "\n\n" (map printTFunDefn funDefnTs)

printTFunDefn :: FunDefnT (Type ByteString) ByteString -> Builder
printTFunDefn (FunDefnT n (Quant qs) (LamT t vs body)) =

    let sig  = TB.intercalate " " [ bytestring n
                                  , ":"
                                  , printPolyType (Forall qs t) ]

        impl = TB.intercalate " " [ bytestring n
                                  , printVars vs
                                  , "=\n" ]

    in TB.intercalate "\n" [sig, impl <> printTypedExpression 1 body]

 -- TODO quantified?
printTFunDefn (FunDefnT n _ expr) =

    let sig = TB.intercalate " " [ bytestring n
                                 , ":"
                                 , printType (typeOf expr) ]

        impl = TB.intercalate " " [ bytestring n
                                  , "=\n"
                                  , printTypedExpression 1 expr ]

    in TB.intercalate "\n" [sig, impl]

printTypedExpression :: Int
                     -> ExprT (Type ByteString) ByteString
                     -> Builder

printTypedExpression ind aexp =

    case aexp of

        (TermT t term) ->
            TB.intercalate " " [ "(" <> printTerm (decodeUtf8 <$> term)
                               , ":"
                               , printType t <> ")"]

        (LamT _ vs body) ->
            let body' = printTypedExpression ind body
            in
            mconcat ["(\\", printVars vs, ". ", body', ")"]

        (AppT t f xs) ->
            let f'  = printTypedExpression ind f
                xs' = map (printTypedExpression ind) xs
            in mconcat ["(", TB.intercalate " " (f':xs'), " : ", printType t, ")"]

        (LetT _ a b c) ->
            let a' = mconcat ["(", bytestring a, " : ", printType (typeOf b), ")"]
            in TB.intercalate "\n" [ indent ind <> "let " <> a' <> " = " <> printTypedExpression ind b <> " in"
                                   , indent ind <> printTypedExpression ind c ]

        (UnPrimOpT _ op a) ->
            TB.intercalate " " [ printUnOp op
                               , printTypedExpression ind a ]
        (BinPrimOpT _ op a b) ->
            TB.intercalate " " [ printTypedExpression ind a
                               , printBinOp op
                               , printTypedExpression ind b ]

        (IfThenElseT _ pr tr fl) ->
            TB.intercalate "\n" [ indent  ind    <> "if "   <> printTypedExpression  ind    pr
                                , indent (ind+2) <> "then " <> printTypedExpression (ind+2) tr
                                , indent (ind+2) <> "else " <> printTypedExpression (ind+2) fl ]

        (CaseT _ scrut patterns) ->
            TB.intercalate "\n" ( indent ind <> "case " <> printTypedExpression ind scrut <> " of"
                                : map (printPattern (ind+1)) patterns )

printPattern :: Int
             -> PatternT (Type ByteString) ByteString
             -> Builder
printPattern ind (PatternT lhs rhs) =
    mconcat [ indent ind
            , printTypedExpression ind lhs
            , " -> "
            , printTypedExpression ind rhs ]
