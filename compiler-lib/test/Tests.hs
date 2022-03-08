import Hedgehog (checkParallel)

import Parse.ByteStringTest (byteStringTests)
import Parse.LexerTest      (lexerTests)

main :: IO ()
main = mapM_ checkParallel [ byteStringTests
                           , lexerTests
                           ]
