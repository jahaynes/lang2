{-# LANGUAGE OverloadedStrings #-}

module BetterPretty.UglyModule where

import Core.Definition
import TypeCheck.Types

import           Data.ByteString    (ByteString)
import           Data.Text          (Text, pack)
import qualified Text.Builder as TB

moduleToText :: Module ByteString -> Text
moduleToText md =
    let dataDefs = map (TB.text . pack . show) $ getDataDefns md
        funDefs  = map (TB.text . pack . show) $ getFunDefns md
    in TB.run . TB.intercalate "\n\n" $ concat [dataDefs, funDefs]

typedModuleToText :: TypedModule Scheme ByteString -> Text
typedModuleToText md =
    let dataDefs = map (TB.text . pack . show) $ getDataDefnsT md
        funDefs  = map (TB.text . pack . show) $ getFunDefnsT md
    in TB.run . TB.intercalate "\n\n" $ concat [dataDefs, funDefs]
