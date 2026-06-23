{-# LANGUAGE OverloadedStrings,
             QuasiQuotes #-}

module Pretty.Direct where

import Phase.CodeGen.Direct

import qualified Data.ByteString.Char8 as C8
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.String.Interpolate (i)

renderCodeGen :: Code -> Text
renderCodeGen (Code fs asm) = T.unlines
                            . concat 
                            $ [ map renderFunc fs
                              , map renderAsm asm ]

renderFunc :: Func -> Text
renderFunc (Func name vs body) =
    [i|#{name}(#{C8.intercalate ", " vs}) {\n#{T.unlines $ map renderAsm body}}|]

renderAsm :: Asm -> Text
renderAsm asm = [i|    #{asm}|]
