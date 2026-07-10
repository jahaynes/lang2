{-# LANGUAGE OverloadedStrings #-}

module Pretty.Anf where

import Common.State
import Core.Term
import Phase.Anf.Anf
import Pretty.Common
import Pretty.Operator

import           Data.ByteString       (ByteString)
import           Data.Functor          ((<&>))
import           Data.Text             (Text)
import qualified Data.Text as T
import           TextBuilder           (TextBuilder)
import qualified TextBuilder as TB

renderAnfModule :: AnfModule ByteString -> Text
renderAnfModule = TB.toText . printAnfModule

printAnfModule :: AnfModule ByteString -> TextBuilder
printAnfModule (AnfModule _ funDefns) = TB.intercalate "\n\n" (map printAnfFunDefn funDefns)

printAnfFunDefn :: FunDefAnfT ByteString -> TextBuilder
printAnfFunDefn (FunDefAnfT n _qtodo t evs vs expr) =

    let typ = TB.string . show $ t -- "some type" -- error "TODO type" -- printPolyType (Forall qs (typeOf expr))
        sig = bytestring n <> " : " <> typ
        vars = case vs of
                   [] -> ""
                   _  -> bytestring " " <> (TB.intercalate " " $ map bytestring vs)

        evars = case evs of
                   [] -> ""
                   _  -> bytestring " {" <> (TB.intercalate " " $ map bytestring evs) <> bytestring "} "

        impl = evalState (printNExp expr) 2

    in TB.intercalate "\n" [ sig
                           , bytestring n <> evars <> vars <> " ="
                           , impl ]

withIndent :: State Int TextBuilder -> State Int TextBuilder
withIndent sf = State $ \i -> (evalState sf (i+2), i)

noIndent :: State Int TextBuilder -> State Int TextBuilder
noIndent sf = State $ \i -> (evalState sf 0, i)

repl :: Int -> Text -> TextBuilder
repl n = TB.text . T.replicate n

indentSt :: TextBuilder -> State Int TextBuilder
indentSt b = get <&> \i -> repl i " " <> b

printNExp :: NExp ByteString -> State Int TextBuilder
printNExp nexp =

    case nexp of

        AExp aexp ->
            printAExp aexp

        CExp cexp ->
            printCExp cexp

        NLet _ a b c -> do
            a' <- indentSt $ "let " <> bytestring a <> " = "
            b' <- noIndent $ printNExp b
            c' <- printNExp c
            pure $ mconcat [a', b', " in\n", c']

printAExp :: AExp ByteString -> State Int TextBuilder
printAExp aexp =

    case aexp of

        ATerm _ term ->
            printTerm term

printCExp :: CExp ByteString -> State Int TextBuilder
printCExp cexp =

    case cexp of

        CApp _ f xs -> do
            f'  <- printAExp f
            xs' <- mapM (noIndent . printAExp) xs
            pure $ f' <> "(" <> TB.intercalate "," xs' <> ")"

        CAppClo _ f (AClosEnv cloEnv) xs -> do
            f'    <- printAExp f
            env'  <- pure $ "{" <> TB.intercalate " " (map bytestring cloEnv) <> "}"
            xs'   <- mapM (noIndent . printAExp) xs
            pure $ f' <> env' <> "(" <> TB.intercalate "," xs' <> ")"

        CUnPrimOp _ op a -> do
            a' <- printAExp a
            pure $ TB.intercalate " " [printUnOp op, a']

        CBinPrimOp _ op a b -> do
            a' <- printAExp a
            b' <- noIndent $ printAExp b
            pure $ TB.intercalate " " [a', printBinOp op, b']

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

printPExp :: PExp ByteString -> State Int TextBuilder
printPExp (PExp lhs rhs) = do
    lhs' <- noIndent $ printPPat lhs
    rhs' <- noIndent $ printNExp rhs
    pure $ TB.intercalate " " [lhs', "->", rhs']

printPPat :: PPat ByteString -> State Int TextBuilder
printPPat (PVar v) = pure $ bytestring v
printPPat (PApp dc _ ps) = do
    ps' <- mapM printPPat ps
    pure $ TB.intercalate " " (bytestring dc:ps')

printTerm :: Term ByteString -> State Int TextBuilder
printTerm term =
    indentSt $ case term of
                   LitBool b   -> if b then "True" else "False"
                   LitInt i    -> TB.decimal i
                   LitString s -> mconcat ["\"", bytestring s, "\""]
                   Var v       -> bytestring v
                   DCons dc    -> bytestring dc
