module Core.Definition where

import Core.Expression

data Defn s =
    Defn s (Expr s)
        deriving (Eq, Show)
