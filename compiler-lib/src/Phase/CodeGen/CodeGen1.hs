module Phase.CodeGen.CodeGen1 (codeGenModule1, renderCodeGen1) where

import Phase.CodeGen.CodeGen0 (Instr (..), SubRoutine (..))

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import qualified Data.Text as T

renderCodeGen1 :: [Instr ByteString] -> Text
renderCodeGen1 = T.unlines
               . map (\(ln, instr) -> T.pack $ show ln ++ ":\t" ++ show instr)
               . zip [(0::Int)..]

codeGenModule1 :: [SubRoutine s] -> [Instr s]
codeGenModule1 = concatMap toInstrs
    where
    toInstrs sub = ILabel (getName sub) : getInstrs sub

-- TODO - go all the way to general purpose registers now?
-- popping from the stack into the stack doesn't really make sense
-- maybe just ignore pops for now?  Need to unwind tho.  use sp, bp?
