{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen0 (SubRoutine, codeGenModule0, renderCodeGen0) where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))

import           Control.Monad         (replicateM)
import           Data.ByteString.Char8 (ByteString, pack)
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

data Instr s = CallFun (Val s)
             | PushArg (Val s)
             | Pop s
             | Ret (Val s)
             | BinOpInstr BinOp s (Val s) (Val s)
             | Assign s (Val s)
             | Cmp (Val s)
             | Jmp s
             | JmpNeq s
             | ILabel s
               deriving Show

data Val s = Reg s
           | VInt Integer
           | VBool Bool
           | Label s
           | VDConsName s
           | VDCons s [Val s] -- TODO include tag?
               deriving Show

renderCodeGen0 :: [SubRoutine ByteString] -> Text
renderCodeGen0 = T.unlines . map render
    where
    render (SubRoutine n is) =
        T.unlines ( decodeUtf8 n <> ":"
                  : (map (\i -> "  " <> i) $ map (T.pack . show) is)
                  )

codeGenModule0 :: AnfModule ByteString -> [SubRoutine ByteString]
codeGenModule0 = map (process genFresh assignReg) . getFunDefAnfTs

    where
    genFresh :: FreshType -> State (GenState ByteString) ByteString
    genFresh ft = do
        st <- get
        let num = freshNum st
        put st { freshNum = num + 1 }

        let pr = case ft of
                     FrReg         -> "%"
                     FrTrueBranch  -> "tr_"
                     FrFalseBranch -> "fl_"
                     FrJoin        -> "dn_"

        pure $ pr <> pack (show num)

data FreshType = FrReg
               | FrTrueBranch
               | FrFalseBranch
               | FrJoin

assignReg :: ByteString -> State (GenState ByteString) ByteString
assignReg v = do
    st <- get
    let num = freshNum st
        reg = "%" <> pack (show num)
    put st { regMap   = M.insert v reg $ regMap st 
           , freshNum = num + 1 }
    pure reg

process :: (Ord s, Show s) => (FreshType -> State (GenState s) s)
                           -> (s -> State (GenState s) s)
                           -> FunDefAnfT s
                           -> SubRoutine s
process genFresh assignReg (FunDefAnfT name q expr) =

    case expr of

        AExp (ALam _ vs body) ->

            let (instrs, rval) =
                    evalState' (GenState mempty 0) $ do
                        regs <- replicateM (length vs) (genFresh FrReg)
                        let rm = M.fromList $ zip vs regs
                        modify' $ \st -> st { regMap = rm }
                        let pops = map (\v -> Pop $ rm ! v) vs
                        (is, x) <- go genFresh assignReg body
                        pure (pops ++ is, x)

            in SubRoutine { getName   = name
                          , getInstrs = instrs ++ [Ret rval] }

        _ ->
            let asLambda = AExp (ALam (typeOf expr) [] expr)
            in process genFresh assignReg (FunDefAnfT name q asLambda)

go :: (Ord s, Show s) => (FreshType -> State (GenState s) s)
                      -> (s -> State (GenState s) s)
                      -> NExp s
                      -> State (GenState s) ([Instr s], Val s)
go genFresh assignReg = goNexp

    where
    goNexp (AExp aexp)  = goAexp aexp
    goNexp (CExp cexp)  = goCexp cexp
    goNexp (NLet a b c) = do
        a'        <- assignReg a
        (is1, b') <- goNexp b
        (is2, c') <- goNexp c
        pure (is1 ++ [Assign a' b'] ++ is2, c')

    goAexp aexp =

        case aexp of

            ABinPrimOp _ op a b -> do
                (is1, a') <- goAexp a
                (is2, b') <- goAexp b
                fr        <- genFresh FrReg
                pure (is1 ++ is2 ++ [BinOpInstr op fr a' b'], Reg fr)

            ATerm _ term -> do
                val <- goTerm term
                pure ([], val)

            AClo _ _ _ _ -> do
                error "clo"

    goCexp cexp =

        case cexp of

            -- An application can be a function call or a data construction
            CApp _ f xs -> do

                (is1,  f') <- goAexp f

                case f' of

                    -- TODO a nullary DC like Nothing doesn't pass through here

                    -- Data construction (todo - ensure saturation?)
                    VDConsName vcn -> do
                        (is2, xs') <- unzip <$> mapM goAexp xs
                        fr <- genFresh FrReg --reg?

                        pure ( concat [ is1
                                      , concat is2
                                      , [Assign fr (VDCons vcn xs')]
                                      ]
                             , Reg fr )

                    -- Function call
                    Label{} -> do
                        (is2, xs') <- unzip <$> mapM goAexp xs
                        let pushes = map PushArg $ reverse xs'
                        fr <- genFresh FrReg
                        pure ( concat [ is1
                                      , concat is2
                                      , pushes
                                      , [CallFun f', Pop fr]]
                             , Reg fr)

                    Reg{} -> do
                        (is2, xs') <- unzip <$> mapM goAexp xs
                        let pushes = map PushArg $ reverse xs'
                        fr <- genFresh FrReg
                        pure ( concat [ is1
                                      , concat is2
                                      , pushes
                                      , [CallFun f', Pop fr]]
                             , Reg fr)

            CIfThenElse _ pr tr fl -> do

                (is1, v) <- goAexp pr

                trLabel <- genFresh FrTrueBranch
                flLabel <- genFresh FrFalseBranch
                dnLabel <- genFresh FrJoin

                let prs = is1 ++ [ Cmp v
                                 , JmpNeq flLabel
                                 , Jmp trLabel ]

                fr <- genFresh FrReg

                (is2, tr') <- goNexp tr
                let trs = ILabel trLabel : is2 ++ [Assign fr tr', Jmp dnLabel]

                (is3, fl') <- goNexp fl           
                let fls = ILabel flLabel : is3 ++ [Assign fr fl', Jmp dnLabel]

                pure (prs ++ trs ++ fls ++ [ILabel dnLabel], Reg fr)

goTerm :: (Ord s, Show s) => Term s
                          -> State (GenState s) (Val s)
goTerm term =

    case term of

        Var v -> do
            mr <- M.lookup v . regMap <$> get
            pure $ case mr of
                       Just r  -> Reg r
                       Nothing -> Label v

        LitInt i ->
            pure $ VInt i

        LitBool b ->
            pure $ VBool b

        DCons dc ->
            pure $ VDConsName dc
