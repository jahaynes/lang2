module Main where

import Phase.CodeGen.TypesC
import Service.Controller (runController)
import Text.Printf        (printf)

main :: IO ()
main = do
    typesCMain
    let port = 8080
    printf "Running compiler-explorer on port %d\n" port
    runController port
