{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Phase.CodeGen.CodeGen0 (SubRoutine, codeGenModule0, renderCodeGen0) where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))

import           Control.Monad         (replicateM)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Functor          ((<&>))
import           Data.Map.Strict       (Map, (!))
import qualified Data.Map as M
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding    (decodeUtf8)

data GenState s =
    GenState { regMap   :: !(Map s s)
             , freshNum :: !Int }

data SubRoutine s =
    SubRoutine { getName   :: !s
               , getInstrs :: ![Instr s]
               } deriving Show

data Instr s = CallFun s s
             | PushArg (Val s)
             | PopArg s
             | Ret (Val s)
             | BinOpInstr BinOp s (Val s) (Val s)
             | Assign s (Term s)
               deriving Show

data Val s = Reg s
           | VInt Integer
           | VBool Bool
           | Label s
               deriving Show

renderCodeGen0 :: [SubRoutine ByteString] -> Text
renderCodeGen0 = T.unlines . map render

    where
    render (SubRoutine n is) = do
        T.unlines ( decodeUtf8 n <> ":"
                  : (map (\i -> "  " <> i) $ map (T.pack . show) is)
                  )

codeGenModule0 :: AnfModule ByteString -> [SubRoutine ByteString]
codeGenModule0 = map (process genFresh) . getFunDefAnfTs

    where
    genFresh :: State (GenState ByteString) ByteString
    genFresh = do
        st <- get
        let num = freshNum st
        put st { freshNum = num + 1 }
        pure . pack $ "%" ++ show num

getRegFor :: Ord s => s -> State (GenState s) (Maybe s)
getRegFor v =
    M.lookup v . regMap <$> get

process :: (Ord s, Show s) => State (GenState s) s -> FunDefAnfT s -> SubRoutine s
process genFresh (FunDefAnfT name q expr) =

    case expr of

        AExp (ALam _ vs body) ->

            let (instrs, rval) =
                    evalState' (GenState mempty 0) $ do
                        regs <- replicateM (length vs) genFresh
                        let rm = M.fromList $ zip vs regs
                        modify' $ \st -> st { regMap = rm }
                        let pops = map (\v -> PopArg $ rm ! v) vs
                        (is, x) <- goNexp genFresh body
                        pure (pops ++ is, x)

            in SubRoutine { getName   = name
                          , getInstrs = instrs ++ [Ret rval] }

        _ ->
            let asLambda = AExp (ALam (typeOf expr) [] expr)
            in process genFresh (FunDefAnfT name q asLambda)


goNexp :: (Ord s, Show s) => State (GenState s) s
                          -> NExp s
                          -> State (GenState s) ([Instr s], Val s)
goNexp genFresh (AExp aexp) = goAexp genFresh aexp
goNexp genFresh (CExp cexp) = goCexp genFresh cexp
goNexp _ (NLet _a _b _c) = error "let"

goAexp :: (Ord s, Show s) => State (GenState s) s
                          -> AExp s
                          -> State (GenState s) ([Instr s], Val s)
goAexp genFresh aexp =

    case aexp of

        ABinPrimOp _ op a b -> do

            (is1, a') <- goAexp genFresh a
            (is2, b') <- goAexp genFresh b
            fr        <- genFresh
            pure (is1 ++ is2 ++ [BinOpInstr op fr a' b'], Reg fr)

        ATerm _ term -> do
            val <- goTerm term
            pure ([], val)

        _ -> error $ show aexp

goCexp :: (Ord s, Show s) => State (GenState s) s
                          -> CExp s
                          -> State (GenState s) ([Instr s], Val s)
goCexp genFresh cexp =

    case cexp of

        CApp _ f xs -> do
            (is1, Label f') <- goAexp genFresh f
            (is2,      xs') <- unzip <$> mapM (goAexp genFresh) xs
            let pushes = map PushArg $ reverse xs'
            fr <- genFresh
            pure (is1 ++ concat is2 ++ pushes ++ [CallFun fr f'], Reg fr)

        _ -> error $ show cexp

goTerm :: (Ord s, Show s) => Term s
                          -> State (GenState s) (Val s)
goTerm term =

    case term of

        Var v ->
            getRegFor v <&> \case
                Just r  -> Reg r
                Nothing -> Label v

        LitInt i ->
            pure $ VInt i

        LitBool b ->
            pure $ VBool b

