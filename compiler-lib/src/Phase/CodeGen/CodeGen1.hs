{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen1 (codeGenModule1, renderCodeGen1) where

import Phase.CodeGen.CodeGen0 (Instr (..), SubRoutine (..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

renderCodeGen1 :: [Instr ByteString] -> ByteString
renderCodeGen1 = C8.unlines
               . map render
               . zip [(0::Int)..]

    where
    render (_ , ILabel s) = "\n  " <> s <> ":"
    render (_ , IComment s) = "\n  " <> s
    render (ln, instr) = C8.pack $ show ln ++ ":\t" ++ show instr

codeGenModule1 :: [SubRoutine s] -> [Instr s]
codeGenModule1 = concatMap toInstrs
    where
    toInstrs sub = ILabel (getName sub) : getInstrs sub

-- TODO - go all the way to general purpose registers now?
-- popping from the stack into the stack doesn't really make sense
-- maybe just ignore pops for now?  Need to unwind tho.  use sp, bp?
