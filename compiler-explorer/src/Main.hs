module Main where

--import Service.Controller (runController)
--import Text.Printf        (printf)

import Phase.CodeGen.TypesC

main :: IO ()
main = do
    --let port = 8080
    --printf "Running compiler-explorer on port %d\n" port
    --runController port
    typesCMain