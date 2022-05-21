import Hedgehog (checkParallel)

import Common.CallGraphTest    (callGraphTests)
import Parse.ExpressionTest    (exprTests)
import Parse.LexerTest         (lexerTests)
import Parse.LexAndParseTest   (lexAndParseTests)
<<<<<<< HEAD
import TypeCheck.TypeCheckTest (typeCheckTests)
=======
>>>>>>> remove old types implementation

main :: IO ()
main = mapM_ checkParallel [ callGraphTests
                           , exprTests
                           , lexerTests
                           , lexAndParseTests
<<<<<<< HEAD
                           , typeCheckTests
=======
>>>>>>> remove old types implementation
                           ]
