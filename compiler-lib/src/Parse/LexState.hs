module Parse.LexState where

import Parse.Parser (Parser (..))

import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8

type Row = Int
type Col = Int

data SourcePos =
    SourcePos Row Col
        deriving (Eq, Show)

data LexState =
    LexState SourcePos ByteString
        deriving (Eq, Show)

consumedNonNewlineChar :: LexState -> LexState
consumedNonNewlineChar (LexState (SourcePos r c) s) = LexState (SourcePos r (c+1)) (C8.tail s)

withSourcePos :: Parser LexState a -> Parser LexState (SourcePos, a)
withSourcePos parser = do
    sp <- getSourcePos
    x  <- parser
    pure (sp, x)

    where
    getSourcePos :: Parser LexState SourcePos
    getSourcePos = Parser $ \ls@(LexState sp _) -> Right (ls, sp)

alreadyConsumed :: a -> Parser LexState a
alreadyConsumed x = Parser $ \ls -> Right (ls, x)

consumedBs :: LexState -> ByteString -> LexState
consumedBs = go Nothing
    where
    go lastChar acc@(LexState (SourcePos row col) s) bs 
        | C8.null bs = acc
        | otherwise = do
            let char = C8.head bs
            let (row', col') = 
                  case (lastChar, char) of
                    (Just '\r', '\n') -> (row    , col    )
                    (_        , '\r') -> (row + 1, 1      )
                    (_        , '\n') -> (row + 1, 1      )
                    (_        ,    _) -> (row    , col + 1)
            go (Just char) (LexState (SourcePos row' col') (C8.tail s)) (C8.tail bs) 
