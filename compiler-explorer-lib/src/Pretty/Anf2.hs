{-# LANGUAGE OverloadedStrings #-}

module Pretty.Anf2 where

import Common.State
import Core.Module
import Core.Term
import Core.Types
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule
import Pretty.Common
import Pretty.Operator
import Pretty.Type

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor          ((<&>))
import           Data.Text             (Text)
import qualified Data.Text as T
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

renderAnfModule :: AnfModule ByteString -> Text
renderAnfModule = TB.run . printAnfModule

printAnfModule :: AnfModule ByteString -> Builder
printAnfModule (AnfModule _ funDefns) = TB.intercalate "\n\n" (map printAnfFunDefn funDefns)

printAnfFunDefn :: FunDefAnfT ByteString -> Builder
printAnfFunDefn (FunDefAnfT n (Quant qs) expr) =

    let typ = printPolyType (Forall qs (typeOf expr))
        sig = bytestring n <> " : " <> typ
    in case expr of

        AExp (ALam _ vs body) ->
            let vs' = TB.intercalate " " $ map bytestring vs
                impl = evalState (printNExp body) 2
            in TB.intercalate "\n" [ sig
                                   , TB.intercalate " " [bytestring n, vs', "="]
                                   , impl ]

        _ ->
            let impl = evalState (printNExp expr) 2
            in TB.intercalate "\n" [ sig
                                   , bytestring n <> " ="
                                   , impl ]

withIndent :: State Int Builder -> State Int Builder
withIndent sf = State $ \i -> (evalState sf (i+2), i)

noIndent :: State Int Builder -> State Int Builder
noIndent sf = State $ \i -> (evalState sf 0, i)

repl :: Int -> Text -> Builder
repl n = TB.text . T.replicate n

indentSt :: Builder -> State Int Builder
indentSt b = get <&> \i -> repl i " " <> b

printNExp :: NExp ByteString -> State Int Builder
printNExp nexp =

    case nexp of

        AExp aexp ->
            printAExp aexp

        CExp cexp ->
            printCExp cexp

        NLet a b c -> do
            a' <- indentSt $ "let " <> bytestring a <> " = "
            b' <- noIndent $ printNExp b
            c' <- printNExp c
            pure $ mconcat [a', b', " in\n", c']

printAExp :: AExp ByteString -> State Int Builder
printAExp aexp =

    case aexp of

        ATerm _ term ->
            printTerm term

        ALam _ vs body -> do
            body' <- printNExp body
            let vs' = bytestring $ C8.intercalate " " vs
            pure $ mconcat ["(\\", vs', ".", body', ")"]

        AClo _ fvs vs body -> do
            body' <- printNExp body
            let fvs' = bytestring $ C8.intercalate " " (map snd fvs)
                vs'  = bytestring $ C8.intercalate " " vs
            pure $ mconcat ["(\\", vs', " {", fvs', "}.", body', ")"]

        AUnPrimOp _ op a -> do
            a' <- printAExp a
            pure $ TB.intercalate " " [printUnOp op, a']

        ABinPrimOp _ op a b -> do
            a' <- printAExp a
            b' <- noIndent $ printAExp b
            pure $ TB.intercalate " " [a', printBinOp op, b']

        --AClosEnv evs -> do
        --    let evs' = bytestring $ C8.intercalate " " evs
        --    pure $ "{env " <> evs' <> "}"

printCExp :: CExp ByteString -> State Int Builder
printCExp cexp =

    case cexp of

        CApp _ f xs -> do
            f'  <- printAExp f
            xs' <- mapM (noIndent . printAExp) xs
            pure $ TB.intercalate " " (f':xs')

        CAppClo _ f cloEnv xs -> do
            f'  <- printAExp f
            ce' <- noIndent $ printCloEnv cloEnv
            xs' <- mapM (noIndent . printAExp) xs
            pure $ TB.intercalate " " (f':ce':xs')

        -- TODO improve
        CIfThenElse _ pr tr fl -> do
            pr'  <- noIndent $ printAExp pr
            pr'' <- indentSt ("if " <> pr')
            tr'  <- noIndent $ printNExp tr
            tr'' <- withIndent $ indentSt ("then " <> tr')
            fl'  <- noIndent $ printNExp fl
            fl'' <- withIndent $ indentSt ("else " <> fl')
            pure $ TB.intercalate "\n" [pr'', tr'', fl'']

        CCase _ scrut pexps -> do
            scrut'  <- noIndent $ printAExp scrut
            case'   <- indentSt $ TB.intercalate " " ["case", scrut', "of"]
            pexps'  <- mapM printPExp pexps
            pexps'' <- mapM (withIndent . indentSt) pexps'
            pure $ TB.intercalate "\n" (case':pexps'')

printPExp :: PExp ByteString -> State Int Builder
printPExp (PExp lhs rhs) = do
    lhs' <- noIndent $ printPPat lhs
    rhs' <- noIndent $ printNExp rhs
    pure $ TB.intercalate " " [lhs', "->", rhs']

printPPat (PVar v) = pure $ bytestring v
printPPat (PApp dc _ ts) = do
    ts' <- mapM printTerm ts
    pure $ TB.intercalate " " (bytestring dc:ts')

printTerm :: Term ByteString -> State Int Builder
printTerm term =
    indentSt $ case term of
                   LitBool b   -> if b then "True" else "False"
                   LitInt i    -> TB.decimal i
                   LitString s -> mconcat ["\"", bytestring s, "\""]
                   Var v       -> bytestring v
                   DCons dc    -> bytestring dc

printCloEnv :: AClosEnv ByteString -> State Int Builder
printCloEnv (AClosEnv es) = pure
                          . TB.intercalate ", " 
                          . map f
                          $ es
    where
    f x = "{" <> g x <> "}"
    g (TyCon tc [], fv) = TB.intercalate " " [bytestring fv, "::", bytestring tc]