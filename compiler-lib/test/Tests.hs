import Hedgehog (checkParallel)

import Parse2.ByteStringTest (byteStringTests)
import Parse2.Lexer2Test     (lexer2Tests)

main :: IO ()
main = mapM_ checkParallel [ byteStringTests
                           , lexer2Tests
                           ]
