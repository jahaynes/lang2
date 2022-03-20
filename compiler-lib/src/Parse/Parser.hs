{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module Parse.Parser where

import Parse.Token

import           Data.ByteString       (ByteString)
import           Data.IntSet           (IntSet)
import qualified Data.IntSet as IS
import           Data.Vector           (Vector, (!?))
import qualified Data.Vector as V
import           VectorBuilder.Builder (empty, singleton)
import           VectorBuilder.Vector  (build)

class Alternative f where
  (<|>) :: f a -> f a -> f a

newtype Parser s a =
    Parser { runParser :: s -> Either ByteString (s, a) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser run) = Parser $ \s -> (\(s',a) -> (s', f a)) <$> run s

instance Applicative (Parser s) where

    pure :: a -> Parser s a
    pure x = Parser $ \s -> Right (s, x)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> px = Parser $ \s ->
        case runParser pf s of
            Left l        -> Left l
            Right (s', f) ->
                case runParser px s' of
                    Left l         -> Left l
                    Right (ss', a) -> Right (ss', f a)

instance Monad (Parser s) where
    return :: a -> Parser s a
    return = pure

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    ma >>= f = Parser $ \s ->
        case runParser ma s of
            Left l        -> Left l
            Right (s', a) -> runParser (f a) s'

instance Alternative (Parser s) where -- TODO accumulate failure alternatives?
    p1 <|> p2 = Parser $ \s ->
        case runParser p1 s of
            Right r -> Right r
            Left _  ->
                case runParser p2 s of
                    Right r -> Right r
                    Left _  -> Left "no alternatives left"

parse' :: Vector Int
       -> IntSet
       -> Vector Token
       -> Parser ParseState a
       -> Either ByteString a
parse' pos ls s p =
    let ps = ParseState { ps_tokens     = s
                        , ps_pos        = 0
                        , ps_positions  = pos
                        , ps_lineStarts = ls
                        }
    in
    case runParser p ps of
        Left l -> Left l
        Right (s', x) | ps_pos s' == length (ps_tokens s') -> Right x
                      | otherwise                          -> Left "Leftover input"

data ParseState =
    ParseState { ps_tokens     :: !(Vector Token)
               , ps_pos        :: !Int
               , ps_positions  :: Vector Int
               , ps_lineStarts :: IntSet
               } deriving (Eq, Show)

many :: Parser s a -> Parser s (Vector a)
many p = Parser $
    let loop acc s =
          case runParser p s of 
            Left _        -> Right (s, build acc)
            Right (s', x) -> loop (acc <> singleton x) s'
    in loop empty

many' :: Parser s a -> Parser s [a]
many' p = V.toList <$> many p

getColumn :: ParseState -> Maybe Int
getColumn ps = do
    tokenPos      <- ps_positions ps !? ps_pos ps
    prevLineStart <- IS.lookupLE tokenPos (ps_lineStarts ps)
    pure $ tokenPos - prevLineStart

-- TODO, if this passes, why are we not guaranteed a column?
parseWithColumn :: Parser ParseState a -> Parser ParseState (Maybe Int, a)
parseWithColumn p = Parser $ \ps ->
    case runParser p ps of
        Left l -> Left l
        Right (ps', x) -> Right (ps', (getColumn ps, x))

parseWhileColumns :: ColumnComparison -> Parser ParseState a -> Parser ParseState [a]
parseWhileColumns comparison p = Parser $ \ps -> parseWhileColumns' comparison p ps

parseWhileColumns' :: ColumnComparison -> Parser ParseState a -> ParseState -> Either ByteString (ParseState, [a])
parseWhileColumns' comparison p qs =
    case runParser (parseWithColumn p) qs of
        Left l -> Left l
        Right (ps', (mCol1, x)) -> Right (parseWhileColumns'' ps' mCol1 [x])

    where
    parseWhileColumns'' ps    Nothing acc = (ps, reverse acc)
    parseWhileColumns'' ps (Just col) acc =
        case runParser (parseWithColumn p) ps of
            Left _ -> (ps, reverse acc)
            Right (ps', (mCol2, y)) ->
                case mCol2 of
                    -- impossible case?
                    Nothing -> (ps, reverse acc)
                    j@(Just col2)
                        | comparison == MoreRight && col2 <= col -> (ps, reverse acc)
                        | comparison == NotLeft   && col2 <  col -> (ps, reverse acc)
                        | otherwise                              -> parseWhileColumns'' ps' j (y:acc)

parseWhileColumns1 :: ColumnComparison -> Parser ParseState a -> Parser ParseState (a, [a])
parseWhileColumns1 comparison p = Parser $ \ps ->
    case parseWhileColumns' comparison p ps of
        Left l -> Left l
        Right (ps', ys) ->
            case ys of
                [] -> Left "No results for parseWhileColumns1"
                (x:xs) -> Right (ps', (x, xs))

data ColumnComparison = MoreRight
                      | NotLeft
                          deriving Eq