{-# LANGUAGE OverloadedStrings #-}

module Pretty.Anf2 where

import Common.State
import Core.Module
import Core.Term
import Core.Types
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule
import Pretty.Operator

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor          ((<&>))
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Text.Builder          (Builder)
import qualified Text.Builder as TB

renderAnfModule :: AnfModule ByteString -> Text
renderAnfModule = TB.run . printAnfModule

printAnfModule :: AnfModule ByteString -> Builder
printAnfModule (AnfModule _ funDefnTs) = TB.intercalate "\n\n" (map printAnfFunDefn funDefnTs)

printAnfFunDefn :: FunDefAnfT ByteString -> Builder
printAnfFunDefn (FunDefAnfT n (Quant qs) expr) =

    let typ = printType $ typeOf expr
        sig = byteString n <> " : " <> typ
    in case expr of

        AExp (ALam _ vs body) ->
            let vs' = TB.intercalate " " $ map byteString vs
                impl = evalState (printNExp body) 2
            in TB.intercalate "\n" [ sig
                                   , TB.intercalate " " [byteString n, vs', "="]
                                   , impl ]

        _ ->
            let impl = evalState (printNExp expr) 2
            in TB.intercalate "\n" [ sig
                                   , byteString n <> " ="
                                   , impl ]

withIndent :: State Int Builder -> State Int Builder
withIndent sf = State $ \i -> (evalState sf (i+2), i)

noIndent :: State Int Builder -> State Int Builder
noIndent sf = State $ \i -> (evalState sf 0, i)

repl :: Int -> Text -> Builder
repl n = TB.text . T.replicate n

byteString :: ByteString -> Builder
byteString = TB.text . decodeUtf8

indent :: Builder -> State Int Builder
indent b = get <&> \i -> repl i " " <> b

printType :: Type ByteString -> Builder
printType = TB.intercalate " -> " . unbuild []
    where
    unbuild acc (TyArr a b) = unbuild (a:acc) b
    unbuild acc           t = reverse $ map prt (t:acc)

    prt (TyCon c tvs) = TB.intercalate " " (byteString c : map prt tvs)
    prt (TyVar v)     = byteString v
    prt t@TyArr{}     = mconcat ["(", printType t,")"]

printNExp :: NExp ByteString -> State Int Builder
printNExp nexp =

    case nexp of

        AExp aexp ->
            printAExp aexp

        CExp cexp ->
            printCExp cexp

        NLet a b c -> do
            a' <- indent $ "let " <> byteString a <> " = "
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
            let vs' = byteString $ C8.intercalate " " vs
            pure $ mconcat ["(\\", vs', ".", body', ")"]

        AClo _ fvs vs body -> do
            body' <- printNExp body
            let fvs' = byteString $ C8.intercalate " " fvs
                vs'  = byteString $ C8.intercalate " " vs
            pure $ mconcat ["(\\", vs', " {", fvs', "}.", body', ")"]

        AUnPrimOp _ op a -> do
            a' <- printAExp a
            pure $ TB.intercalate " " [printUnOp op, a']

        ABinPrimOp _ op a b -> do
            a' <- printAExp a
            b' <- noIndent $ printAExp b
            pure $ TB.intercalate " " [a', printBinOp op, b']

        AClosEnv evs -> do
            let evs' = byteString $ C8.intercalate " " evs
            pure $ "{env " <> evs' <> "}"

printCExp :: CExp ByteString -> State Int Builder
printCExp cexp =

    case cexp of

        CApp _ f xs -> do
            f'  <- printAExp f
            xs' <- mapM (noIndent . printAExp) xs
            pure $ TB.intercalate " " (f':xs')

        -- TODO improve
        CIfThenElse _ pr tr fl -> do
            pr'  <- noIndent $ printAExp pr
            pr'' <- indent ("if " <> pr')
            tr'  <- noIndent $ printNExp tr
            tr'' <- withIndent $ indent ("then " <> tr')
            fl'  <- noIndent $ printNExp fl
            fl'' <- withIndent $ indent ("else " <> fl')
            pure $ TB.intercalate "\n" [pr'', tr'', fl'']

        CCase _ scrut pexps -> do
            scrut'  <- noIndent $ printAExp scrut
            case'   <- indent $ TB.intercalate " " ["case", scrut', "of"]
            pexps'  <- mapM printPExp pexps
            pexps'' <- mapM (withIndent . indent) pexps'
            pure $ TB.intercalate "\n" (case':pexps'')

printPExp :: PExp ByteString -> State Int Builder
printPExp (PExp lhs rhs) = do
    lhs' <- noIndent $ printNExp lhs
    rhs' <- noIndent $ printNExp rhs
    pure $ TB.intercalate " " [lhs', "->", rhs']

printTerm :: Term ByteString -> State Int Builder
printTerm term =
    indent $ case term of
                 LitBool b   -> if b then "True" else "False"
                 LitInt i    -> TB.decimal i
                 LitString s -> mconcat ["\"", byteString s, "\""]
                 Var v       -> byteString v
                 DCons dc    -> byteString dc
