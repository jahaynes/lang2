import Hedgehog (checkParallel)

import Common.GraphTest        (graphTests)
--import Parse.ExpressionTest    (exprTests)
--import Parse.LexerTest         (lexerTests)
--import Parse.LexAndParseTest   (lexAndParseTests)
--import Phase.EtaExpandTest     (etaExpandTests)
--import TypeCheck.TypeCheckTest (typeCheckTests)

main :: IO ()
main = mapM_ checkParallel [ graphTests
                         --  , exprTests
                         --  , lexerTests
                         --  , lexAndParseTests
                         --  , typeCheckTests
                         --  , etaExpandTests
                           ]
