import Hedgehog (checkParallel)

import Parse.ExpressionTest  (exprTests)
import Parse.LexerTest       (lexerTests)
import Parse.LexAndParseTest (lexAndParseTests)

main :: IO ()
main = mapM_ checkParallel [ exprTests
                           , lexerTests
                           , lexAndParseTests
                           ]
