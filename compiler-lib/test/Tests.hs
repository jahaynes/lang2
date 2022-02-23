import Hedgehog (checkParallel)

import Parse.LexerTest  (lexerTests)
import Parse.ParserTest (parserTests)

main :: IO ()
main = mapM_ checkParallel [ lexerTests
                           , parserTests
                           ]
