{-# LANGUAGE OverloadedStrings #-}

module Pretty.Module where

import Core.Definition
import Pretty.Definition
import TypeCheck.Types

import           Data.ByteString    (ByteString)
import           Data.Text          (Text, pack)
import           Data.Text.Encoding (decodeUtf8)
import qualified Text.Builder as TB

render :: Module ByteString -> Text
render md =
    let txtModule = fmap decodeUtf8 md
        dataDefs  = map printDataDefn $ getDataDefns txtModule
        funDefs   = map printFunDefn  $ getFunDefns txtModule
    in TB.run . TB.intercalate "\n\n" $ concat [dataDefs, funDefs]

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
