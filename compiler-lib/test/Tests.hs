import Hedgehog (checkParallel)

import Common.CallGraphTest    (callGraphTests)
import Parse.ExpressionTest    (exprTests)
import Parse.LexerTest         (lexerTests)
import Parse.LexAndParseTest   (lexAndParseTests)
import TypeCheck.TypeCheckTest (typeCheckTests)
import Phase.EtaExpandTest     (etaExpandTests)

main :: IO ()
main = mapM_ checkParallel [ callGraphTests
                           , exprTests
                           , lexerTests
                           , lexAndParseTests
                           , typeCheckTests
                           , etaExpandTests
                           ]
