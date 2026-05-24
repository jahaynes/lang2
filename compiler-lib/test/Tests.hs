import Hedgehog (checkParallel)

import Common.CallGraphTest               (callGraphTests)
import Common.TransTest                   (transTests)
import Parse.ExpressionTest               (exprTests)
import Parse.LexerTest                    (lexerTests)
import Parse.LexAndParseTest              (lexAndParseTests)
import Phase.ClosureConvert.FreeVarsTest  (freeVarsTests)
import Phase.CodeGen.CodeGenDLifetimeTest (codeGenDLifetimeTests)
import Phase.EtaExpandTest                (etaExpandTests)
import TypeCheck.TypeCheckTest            (typeCheckTests)

main :: IO ()
main = mapM_ checkParallel [ callGraphTests
                           , transTests
                           , exprTests
                           , lexerTests
                           , lexAndParseTests
                           , freeVarsTests
                           , codeGenDLifetimeTests
                           , typeCheckTests
                           , etaExpandTests
                           ]
