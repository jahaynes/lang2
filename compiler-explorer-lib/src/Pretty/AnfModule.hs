{-# LANGUAGE OverloadedStrings #-}

module Pretty.AnfModule (renderAnfModule) where

import Core.Module
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule
import Pretty.Common
import Pretty.Operator
import Pretty.Term
import Pretty.Type

import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import           Data.Text.Encoding
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

renderAnfModule :: AnfModule ByteString -> Text
renderAnfModule = TB.run . printAnfModule

printAnfModule :: AnfModule ByteString -> Builder
printAnfModule (AnfModule _ funDefnTs) = TB.intercalate "\n\n" (map printAnfFunDefn funDefnTs)

printAnfFunDefn :: FunDefAnfT ByteString -> Builder
printAnfFunDefn (FunDefAnfT n (Quant qs) expr) =

    let quant = case qs of
                    [] -> ""
                    _  -> "forall " <> TB.intercalate " " (map bytestring qs) <> ". "

        sig = mconcat [bytestring n, " : ", quant, printType $ typeOf expr] in

    case expr of

        AExp (ALam _ vs body) ->

            let impl = TB.intercalate " " [ bytestring n
                                          , printVars vs
                                          , "=\n" ]
            in TB.intercalate "\n" [sig, impl <> printAnfExpression 1 body]

        _ ->
            let impl = TB.intercalate " " [ bytestring n
                                          , "=\n"
                                          , printAnfExpression 1 expr ]
            in TB.intercalate "\n" [sig, impl]

printAnfExpression :: Int
                   -> NExp ByteString
                   -> Builder
printAnfExpression ind expr =

    case expr of

        AExp aexp ->
            printAExp ind aexp

        CExp cexp ->
            printCExp ind cexp

        NLet a b c ->
            TB.intercalate "\n" [ indent ind <> "let " <> bytestring a <> " = " <> printAnfExpression ind b <> " in"
                                , indent ind <> printAnfExpression ind c ]

printAExp :: Int
          -> AExp ByteString
          -> Builder
printAExp ind aexp =

    case aexp of

        ATerm _ term ->
            printTerm (decodeUtf8 <$> term)

        ALam _ vs body ->
            let body' = printAnfExpression ind body
            in
            mconcat ["(\\", printVars vs, ". ", body', ")"]

        AClo _ fvs vs body ->
            let fvs'  = mconcat ["{", printVars fvs, "}"]
                vs'   = printVars vs
                body' = printAnfExpression ind body
            in mconcat ["(\\", fvs', " ", vs', ". ", body', ")"]

        AUnPrimOp _ op a ->
            TB.intercalate " " [ printUnOp op
                               , printAExp ind a ]

        ABinPrimOp _ op a b ->
            let x = TB.intercalate " " [ printAExp ind a
                                       , printBinOp op
                                       , printAExp ind b ]
            in mconcat ["(", x, ")"]

printCExp :: Int
          -> CExp ByteString
          -> Builder
printCExp ind cexp =

    case cexp of

        CApp _ f xs ->
            let f'  = printAExp ind f
                xs' = map (printAExp ind) xs
            in TB.intercalate " " (f':xs')

        CIfThenElse _ pr tr fl ->
            TB.intercalate "\n" [ indent  ind    <> "if "   <> printAExp ind pr
                                , indent (ind+2) <> "then " <> printAnfExpression (ind+2) tr
                                , indent (ind+2) <> "else " <> printAnfExpression (ind+2) fl ]

        -- TODO
        CCase _ scrut ps ->
            TB.intercalate "\n" ( ("case " <> printAExp 0 scrut)
                                  : map (printPExp (ind + 2)) ps )

printPExp :: Int
          -> PExp ByteString
          -> Builder
printPExp ind (PExp a b) = mconcat [ printAnfExpression ind a
                                   , " -> "
                                   , printAnfExpression ind b ]
