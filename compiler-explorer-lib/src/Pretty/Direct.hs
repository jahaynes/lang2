{-# LANGUAGE OverloadedStrings,
             QuasiQuotes #-}

module Pretty.Direct where

import Phase.CodeGen.Direct

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.String.Interpolate (i)

renderCodeGen :: Code -> Text
renderCodeGen (Code fs asm) = do
    
    --let fs' = unlines . map show $ fs
    
    --let asm' = unlines . map show $ asm

    -- pack $ fs' ++ asm'

    T.unlines . concat $ [ map renderFunc fs
                         , map renderAsm asm
                         ]

renderFunc :: Func -> Text
renderFunc (Func name vs body) =
    [i|#{name}(#{vs}(#{T.unlines $ map renderAsm body}))|]

renderAsm :: Asm -> Text
renderAsm = T.pack . show

{-

-- | A compiled top-level function: its name, formal parameters and body.
data Func = Func ByteString [ByteString] [Asm]
    deriving (Eq, Show)


-}