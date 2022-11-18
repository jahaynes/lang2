{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen0 (Instr (..), SubRoutine (..), Val (..), codeGenModule0, renderCodeGen0) where

import Common.State
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.SizeInfo
import Phase.CodeGen.TagInfo
import Phase.CodeGen.Val

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
             | Push (Val s)
             | Pop s
             | Ret
             | UnOpInstr UnOp s (Val s)
             | BinOpInstr BinOp s (Val s) (Val s)
             | Assign s (Val s)
             | Cmp (Val s)
             | JmpLbl s
             | JmpNeqLbl s
             | ILabel s
             | Malloc s Int -- resulting register and size of allocation
             | Cpy (Val s) (Val s)
             | IComment s
               deriving Show

data Deps s =
    Deps { genFresh  :: !(FreshType -> State (GenState s) s)
         , assignReg :: !(s -> State (GenState s) s)
         , allocate  :: !((AExp s, Val s) -> State (GenState s) ([Instr s], Val s))
         , comment   :: !(ByteString -> Instr s)
         , dataDefns :: ![DataDefn s]
         }

renderCodeGen0 :: [SubRoutine ByteString] -> Text
renderCodeGen0 = T.unlines . map render
    where
    render (SubRoutine n is) =
        T.unlines ( decodeUtf8 n <> ":"
                  : (map (\i -> "  " <> i) $ map (T.pack . show) is)
                  )

codeGenModule0 :: AnfModule ByteString -> [SubRoutine ByteString]
codeGenModule0 md = map (process deps)
                  $ getFunDefAnfTs md
    where
    deps = Deps genFreshImpl
                assignRegImpl
                (allocateImpl genFreshImpl)
                IComment
                (getDataDefnAnfTs md)

-- generate a type with this register, so that reads know what to read?
-- is there a point in returning the type if the type is always passed in?
genFreshImpl :: FreshType -> State (GenState ByteString) ByteString
genFreshImpl ft = do
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

unkn :: Type ByteString
unkn = TyCon "idk"

-- the val is what's being allocated, but the AExp still has the type
-- no longer necessary?
allocateImpl :: Monad m => (FreshType -> m ByteString)
                        -> (AExp ByteString, Val ByteString)
                        -> m ([Instr ByteString], Val ByteString)
allocateImpl genFresh (aexp, v) = do

    fr <- genFresh FrReg

    case typeOfAExp aexp of

        TyCon "Int" ->
            pure ( [ Malloc fr 8
                   , Cpy (VAddressAt fr) v]
                 , RegPtr fr )

        TyCon _ -> -- assume its a pointer size? TODO remove
            pure ( [ Malloc fr 8
                   , Cpy (VAddressAt fr) v]
                 , RegPtr fr )

assignRegImpl :: ByteString -> State (GenState ByteString) ByteString
assignRegImpl v = do
    st <- get
    let num = freshNum st
        reg = "%" <> pack (show num)
    put st { regMap   = M.insert v reg $ regMap st 
           , freshNum = num + 1 }
    pure reg

process :: (Show s, Ord s) => Deps s -> FunDefAnfT s -> SubRoutine s
process deps (FunDefAnfT name q expr) =

    case expr of

        AExp (ALam _ vs body) ->

            let (instrs, rval) =
                    evalState' (GenState mempty 0) $ do
                        regs <- replicateM (length vs) (genFresh deps FrReg)
                        let rm = M.fromList $ zip vs regs
                        modify' $ \st -> st { regMap = rm }
                        let pops = map (\v -> Pop $ rm ! v) vs
                        (is, x) <- process' deps body
                        pure (pops ++ is, x)

            in SubRoutine { getName   = name
                          , getInstrs = instrs ++ [Push rval, Ret] }

        _ ->
            let asLambda = AExp (ALam (typeOf expr) [] expr)
            in process deps (FunDefAnfT name q asLambda)

process' :: (Ord s, Show s) => Deps s -> NExp s -> State (GenState s) ([Instr s], Val s)
process' deps = goNexp

    where
    goNexp (AExp aexp)  = goAexp aexp
    goNexp (CExp cexp)  = goCexp cexp
    goNexp (NLet a b c) = do
        a'        <- assignReg deps a
        (is1, b') <- goNexp b
        (is2, c') <- goNexp c
        pure (is1 ++ [Assign a' b'] ++ is2, c')

    goAexp aexp =

        case aexp of

            AUnPrimOp _ op a -> do
                (is1, a') <- goAexp a
                fr        <- genFresh deps FrReg
                pure (is1 ++ [UnOpInstr op fr a'], Reg fr)

            ABinPrimOp _ op a b -> do
                (is1, a') <- goAexp a
                (is2, b') <- goAexp b
                fr        <- genFresh deps FrReg
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

                (is1, f') <- goAexp f

                case f' of

                    -- a nullary DC like Nothing doesn't pass through here
                    -- probably OK - a nullary DC can just be a tag on stack?

                    -- Data construction (todo - ensure saturation?)
                    -- `typeOfCExp cexp` relies on this.  should it be the check too?
                    VDConsName name -> do

                        (is2, xs') <- unzip <$> mapM goAexp xs

                        let Tag tag = getTag (dataDefns deps) (typeOfCExp cexp) name

                        (fr, assignment) <- assignToStruct deps (VDCons name tag xs')

                        pure ( concat [ is1
                                      , concat is2
                                      , [comment deps $ "Start making DataCons " <> pack (show name)]
                                      , assignment
                                      ]
                             , fr )

                    -- Function call to label -- TODO dedupe
                    Label{} -> do
                        (is2, xs') <- unzip <$> mapM goAexp xs
                        let pushes = map Push $ reverse xs'
                        fr <- genFresh deps FrReg
                        pure ( concat [ is1
                                      , concat is2
                                      , pushes
                                      , [CallFun f', Pop fr]]
                             , Reg fr)

                    -- Function call to reg -- TODO dedupe
                    Reg{} -> do
                        (is2, xs') <- unzip <$> mapM goAexp xs
                        let pushes = map Push $ reverse xs'
                        fr <- genFresh deps FrReg
                        pure ( concat [ is1
                                      , concat is2
                                      , pushes
                                      , [CallFun f', Pop fr]]
                             , Reg fr)

            CIfThenElse _ pr tr fl -> do

                (is1, v) <- goAexp pr

                trLabel <- genFresh deps FrTrueBranch
                flLabel <- genFresh deps FrFalseBranch
                dnLabel <- genFresh deps FrJoin

                let prs = is1 ++ [ Cmp v
                                 , JmpNeqLbl flLabel
                                 , JmpLbl trLabel ]

                fr <- genFresh deps FrReg

                (is2, tr') <- goNexp tr
                let trs = ILabel trLabel : is2 ++ [Assign fr tr', JmpLbl dnLabel]

                (is3, fl') <- goNexp fl           
                let fls = ILabel flLabel : is3 ++ [Assign fr fl', JmpLbl dnLabel]

                pure (prs ++ trs ++ fls ++ [ILabel dnLabel], Reg fr)

assignToStruct :: Show s => Deps s -> Val s -> State (GenState s) (Val s, [Instr s])
assignToStruct deps c@(VDCons name tag xs) = do

    let sz = getSize (SizedVal c)

    fr <- genFresh deps FrReg

    let destOffsets = init
                    . scanl (+) 0
                    $ 8 : map (getSize . SizedVal) xs

    -- fr points to the start of malloc.  destOffsets relative to it

    let instrs = map (toInstr fr)
               . zip destOffsets
               $ VTag tag:xs

    pure ( Reg fr
         , Malloc fr sz : instrs )

    where
    toInstr fr (o, v) = Cpy (RegPtrOff fr o) v

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
