import Hedgehog (checkParallel)

import Parse.LexerTest       (lexerTests)
import Parse.ParserTest      (parserTests)
import Parse2.ByteStringTest (byteStringTests)
import Parse2.Lexer2Test     (lexer2Tests)

main :: IO ()
main = mapM_ checkParallel [ byteStringTests
                           , lexerTests
                           , lexer2Tests
                           , parserTests
                           ]
