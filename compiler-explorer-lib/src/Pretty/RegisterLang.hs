{-# LANGUAGE OverloadedStrings #-}

module Pretty.RegisterLang (renderRegModule) where

import Phase.CodeGen.RegisterLang

import Common.State
import Core.Types (Type (..))
import Pretty.Common
import Pretty.Operator

import           Data.ByteString       (ByteString)
import           Data.Functor          ((<&>))
import           Data.Text             (Text)
import qualified Data.Text as T
import           TextBuilder           (TextBuilder)
import qualified TextBuilder as TB

---------------------------------------------------------------
-- Top-level rendering
---------------------------------------------------------------

renderRegModule :: RegModule ByteString -> Text
renderRegModule = TB.toText . printRegModule

printRegModule :: RegModule ByteString -> TextBuilder
printRegModule (RegModule _ funDefs) =
    TB.intercalate "\n\n" (map printRegFunDef funDefs)

---------------------------------------------------------------
-- Function definition
---------------------------------------------------------------

printRegFunDef :: RegFunDef ByteString -> TextBuilder
printRegFunDef (RegFunDef n _qtodo t envCount paramCount blocks) =
    let sig = bytestring n <> " : " <> printType t
        envStr = if envCount > 0
                    then " {" <> TB.intercalate " " (map (printReg . R) [0..envCount-1]) <> "}"
                    else ""
        paramStr = if paramCount > 0
                      then " " <> TB.intercalate " " (map (printReg . R) [envCount..envCount+paramCount-1])
                      else ""
        impl = evalState (printBlocks blocks) 1
    in TB.intercalate "\n" [sig, bytestring n <> envStr <> paramStr <> " =", impl]
---------------------------------------------------------------
-- Basic blocks
---------------------------------------------------------------

printBlocks :: [Block ByteString] -> State Int TextBuilder
printBlocks [] = pure ""
printBlocks [b] = printBlock b
printBlocks (b:bs) = do
    b'  <- printBlock b
    bs' <- printBlocks bs
    pure $ b' <> "\n" <> bs'

printBlock :: Block ByteString -> State Int TextBuilder
printBlock (Block l insts term) = do
    l'  <- indentSt $ printLabel l <> ":"
    is' <- mapM (indentSt . printInst) insts
    t'  <- indentSt $ printTerminator term
    pure $ TB.intercalate "\n" (l' : is' ++ [t'])
---------------------------------------------------------------
-- Instructions
---------------------------------------------------------------

printInst :: Inst ByteString -> TextBuilder
printInst inst =
    case inst of
        Move    _ r a       -> printReg r <> " = Move "     <> printATerm a
        UnOp    _ r op a    -> printReg r <> " = UnOp "     <> printUnOp op <> " " <> printReg a
        BinOp   _ r op a b  -> printReg r <> " = BinOp "    <> printBinOp op <> " " <> printReg a <> " " <> printReg b
        Alloc   _ r dcon fs -> printReg r <> " = Alloc "    <> bytestring dcon <> " " <> TB.intercalate " " (map printReg fs)
        Proj    _ r i s     -> printReg r <> " = Proj "     <> printReg s <> "[" <> TB.decimal i <> "]"
        LoadEnv _ r i       -> printReg r <> " = LoadEnv "  <> TB.decimal i
        AllocClos _ r l cs  -> printReg r <> " = AllocClos " <> printLabel l <> " " <> TB.intercalate " " (map printReg cs)

---------------------------------------------------------------
-- Atomic terms
---------------------------------------------------------------

printATerm :: ATerm ByteString -> TextBuilder
printATerm aterm =
    case aterm of
        AVar v       -> bytestring v
        ADCons c     -> bytestring c
        ALitInt  n   -> TB.decimal n
        ALitBool b   -> if b then "True" else "False"
        ALitString s -> mconcat ["\"", bytestring s, "\""]
---------------------------------------------------------------
-- Block terminators
---------------------------------------------------------------

printTerminator :: Terminator ByteString -> TextBuilder
printTerminator term =
    case term of
        Return _ rs          -> "Return " <> TB.intercalate " " (map printReg rs)
        Jump l               -> "Jump "   <> printLabel l
        Branch _ r t e       -> "Branch " <> printReg r <> " " <> printLabel t <> " " <> printLabel e
        Case _ r alts def    -> "Case "   <> printReg r <> " " <> altsStr <> " default: " <> printLabel def
            where altsStr = TB.intercalate " "
                              [ mconcat [printPPat p, " -> ", printLabel l]
                              | (p, l) <- alts ]
        Call _ rs name args l -> "Call "  <> bytestring name <> "(" <> TB.intercalate " " (map printReg rs) <> " " <> TB.intercalate " " (map printReg args) <> ") " <> printLabel l
        CallClos _ rs f env args l -> "CallClos " <> printReg f <> " " <> printReg env <> "(" <> TB.intercalate " " (map printReg rs) <> " " <> TB.intercalate " " (map printReg args) <> ") " <> printLabel l

---------------------------------------------------------------
-- Patterns
---------------------------------------------------------------

printPPat :: PPat ByteString -> TextBuilder
printPPat (PVar v) = bytestring v
printPPat (PApp c _ ps) = TB.intercalate " " (bytestring c : map printPPat ps)

---------------------------------------------------------------
-- Registers and labels
---------------------------------------------------------------

printReg :: R -> TextBuilder
printReg (R i) = TB.char 'R' <> TB.decimal i

printLabel :: L -> TextBuilder
printLabel (L i) = TB.char 'L' <> TB.decimal i

---------------------------------------------------------------
-- Indentation helpers
---------------------------------------------------------------

indentSt :: TextBuilder -> State Int TextBuilder
indentSt b = get <&> \i -> repl i "  " <> b

repl :: Int -> Text -> TextBuilder
repl n = TB.text . T.replicate n

---------------------------------------------------------------
-- Type printing
---------------------------------------------------------------

printType :: Type ByteString -> TextBuilder
printType t =
    case t of
        TyArr a b -> mconcat [box a, " -> ", box b]
        _         -> box t
    where
    box (TyArr a b) = mconcat ["(", box a, " -> ", box b, ")"]
    box (TyCon c ts) = TB.intercalate " " (bytestring c : map box ts)
    box (TyVar v)    = bytestring v