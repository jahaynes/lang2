import Hedgehog (checkParallel)

import Common.CallGraphTest    (callGraphTests)
import Parse.ExpressionTest    (exprTests)
import Parse.LexerTest         (lexerTests)
import Parse.LexAndParseTest   (lexAndParseTests)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
import TypeCheck.TypeCheckTest (typeCheckTests)
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation

main :: IO ()
main = mapM_ checkParallel [ callGraphTests
                           , exprTests
                           , lexerTests
                           , lexAndParseTests
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
                           , typeCheckTests
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
=======
>>>>>>> remove old types implementation
                           ]
