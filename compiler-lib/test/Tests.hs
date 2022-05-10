import Hedgehog (checkParallel)

import Parse.ExpressionTest    (exprTests)
import Parse.LexerTest         (lexerTests)
import Parse.LexAndParseTest   (lexAndParseTests)
import TypeCheck.CallGraphTest (callGraphTests)

main :: IO ()
main = mapM_ checkParallel [ exprTests
                           , lexerTests
                           , lexAndParseTests
                           , callGraphTests
                           ]
