{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen0 (Instr (..), SubRoutine (..), Val (..), codeGenModule0, renderCodeGen0) where

import Common.EitherT (EitherT (..), left)
import Common.State
import Common.StateT
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term
import Core.Types
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.SizeInfo
import Phase.CodeGen.TagInfo
import Phase.CodeGen.Val

import           Control.Monad         (mapAndUnzipM, replicateM)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Map.Strict       (Map, (!))
import qualified Data.Map.Strict as M
import           Debug.Trace           (trace)

data GenState s =
    GenState { regMap   :: !(Map s s)
             , freshNum :: !Int }

data SubRoutine s =
    SubRoutine { getName   :: !s
               , getInstrs :: ![Instr s]
               } deriving Show

data Instr s = CallFun (Val s)
             | Push (Val s)
             | PopTyped (Type s) s
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
             | ClosurePlaceholder
               deriving Show

data Deps s =
    Deps { genFresh     :: !(FreshType -> EitherT ByteString (State (GenState s)) s)
         , assignReg    :: !(s -> EitherT ByteString (State (GenState s)) s)
         , allocate     :: !((AExp s, Val s) -> EitherT ByteString (State (GenState s)) ([Instr s], Val s))
         , comment      :: !(ByteString -> Instr s)
         , dataDefns    :: ![DataDefn s]
         , getTypeOfVal :: !(Val s -> Type s)
         , sToBs        :: !(s -> ByteString)
         }

renderCodeGen0 :: [SubRoutine ByteString] -> ByteString
renderCodeGen0 = C8.unlines . map render
    where
    render (SubRoutine n is) =
        C8.unlines ( n <> ":"
                  : (map ("  " <>) $ map (C8.pack . show) is)
                  )

codeGenModule0 :: AnfModule ByteString -> Either ByteString [SubRoutine ByteString]
codeGenModule0 md = do
    mapM (process deps) (getFunDefAnfTs md)
    where
    deps = Deps genFreshImpl
                assignRegImpl
                (allocateImpl genFreshImpl)
                IComment
                (getDataDefnAnfTs md)
                typeOfVal
                id

-- generate a type with this register, so that reads know what to read?
-- is there a point in returning the type if the type is always passed in?
-- Don't need whole EitherT
genFreshImpl :: FreshType -> EitherT e (State (GenState ByteString)) ByteString
genFreshImpl ft = do
    st <- lift get
    let num = freshNum st
    lift $ put st { freshNum = num + 1 }
    let pr = case ft of
                    FrReg         -> "%"
                    FrTrueBranch  -> "tr_"
                    FrFalseBranch -> "fl_"
                    FrJoin        -> "dn_"
    pure $ pr <> C8.pack (show num)

data FreshType = FrReg
               | FrTrueBranch
               | FrFalseBranch
               | FrJoin

-- the val is what's being allocated, but the AExp still has the type
-- no longer necessary?
allocateImpl :: Monad m => (FreshType -> m ByteString)
                        -> (AExp ByteString, Val ByteString)
                        -> m ([Instr ByteString], Val ByteString)
allocateImpl genFresh (aexp, v) = do

    fr <- genFresh FrReg

    let t = typeOfAExp aexp

    case t of

        TyCon "Int" _ ->
            pure ( [ Malloc fr 8
                   , Cpy (VAddressAt fr) v]
                 , TypedRegPtr t fr ) -- TODO check 't'

        TyCon _ _ -> -- assume its a pointer size? TODO remove
            pure ( [ Malloc fr 8
                   , Cpy (VAddressAt fr) v]
                 , TypedRegPtr t fr ) -- TODO check 't'

-- Don't need the whole EitherT
assignRegImpl :: ByteString -> EitherT e (State (GenState ByteString)) ByteString
assignRegImpl v = do
    st <- lift get
    let num = freshNum st
        reg = "%" <> C8.pack (show num)
    lift $ put st { regMap   = M.insert v reg $ regMap st
                  , freshNum = num + 1 }
    pure reg

process :: (Show s, Ord s) => Deps s -> FunDefAnfT s -> Either ByteString (SubRoutine s)
process deps (FunDefAnfT name q expr) =

    case expr of

        AExp (ALam t vs body) -> do

            let vts = getParamTypes vs t

            (instrs, rval) <- evalState' (GenState mempty 0) $ runEitherT $ do
                regs <- replicateM (length vs) (genFresh deps FrReg)
                let rm = M.fromList $ zip vs regs
                lift $ modify' $ \st -> st { regMap = rm }
                let pops = map (\(t, v) -> PopTyped t (rm ! v)) $ zip vts vs
                (is, x) <- process' deps body
                pure (pops ++ is, x)

            pure SubRoutine { getName   = name
                            , getInstrs = instrs ++ [Push rval, Ret] }

        _ ->
            let asLambda = AExp (ALam (typeOf expr) [] expr)
            in process deps (FunDefAnfT name q asLambda)

-- surely this will be reused
getParamTypes = go []
    where
    go acc     []           _ = reverse acc
    go acc (_:vs) (TyArr a b) = go (a:acc) vs b

process' :: (Ord s, Show s) => Deps s
                            -> NExp s
                            -> EitherT ByteString (State (GenState s)) ([Instr s], Val s)
process' deps = goNexp

    where
    goNexp (AExp aexp)  = goAexp aexp
    goNexp (CExp cexp)  = goCexp cexp
    goNexp (NLet a b c) = do
        a'        <- assignReg deps a
        (is1, b') <- goNexp b
        (is2, c') <- goNexp c
        pure ( is1 ++ [Assign a' b'] ++ is2
             , c' )

    goAexp aexp =

        case aexp of

            AUnPrimOp t op a -> do
                (is1, a') <- goAexp a
                fr        <- genFresh deps FrReg
                pure ( is1 ++ [UnOpInstr op fr a']
                     , TypedReg t fr)

            ABinPrimOp t op a b -> do
                (is1, a') <- goAexp a
                (is2, b') <- goAexp b
                fr        <- genFresh deps FrReg
                pure ( is1 ++ is2 ++ [BinOpInstr op fr a' b']
                     , TypedReg t fr )

            ATerm t term -> do
                val <- goTerm t term
                pure ([], val)

            AClo t fvs vs body -> do

                (bodyInstrs, rBody) <- goNexp body

                st <- lift get

                pure ( comment deps "pop ptr_env"
                     : comment deps "pop formal parameters"
                     : comment deps "bind env parameters to registers"
                     : bodyInstrs ++
                     [], rBody)

            AClosEnv -> do
                fr <- genFresh deps FrReg
                pure ( comment deps "allocate an env"
                     : comment deps "copy each param in"
                     : [], UntypedReg fr )

    goCexp cexp =

        case cexp of

            -- An application can be a function call or a data construction
            CApp t f xs -> do

                (is1, f') <- goAexp f

                case f' of

                    -- a nullary DC like Nothing doesn't pass through here
                    -- probably OK - a nullary DC can just be a tag on stack?

                    -- Data construction (todo - ensure saturation?)
                    -- `typeOfCExp cexp` relies on this.  should it be the check too?
                    VDConsNameTyped t name -> do

                        (is2, xs') <- mapAndUnzipM goAexp xs

                        let Tag tag = getTag (dataDefns deps) (typeOfCExp cexp) name

                        (fr, assignment) <- assignToStruct deps (VDConsTyped t name tag xs')

                        pure ( concat [ is1
                                      , concat is2
                                      , [comment deps $ "Start making DataCons " <> C8.pack (show name)]
                                      , assignment
                                      ]
                             , fr )

                    -- Function call to label -- TODO dedupe
                    Label{} -> do
                        (is2, xs') <- mapAndUnzipM goAexp xs
                        let pushes = map Push $ reverse xs'
                        fr <- genFresh deps FrReg
                        pure ( concat [ is1
                                      , concat is2
                                      , pushes
                                      , [CallFun f', PopTyped t fr]] -- check correct t
                             , TypedReg t fr) -- TODO Check this is correct 't'

                    -- TODO deduplicate/remove untyped reg
                    TypedReg{} -> do
                        (is2, xs') <- mapAndUnzipM goAexp xs
                        let pushes = map Push $ reverse xs'
                        fr <- genFresh deps FrReg
                        pure ( concat [ is1
                                      , concat is2
                                      , pushes
                                      , [CallFun f', PopTyped t fr]] -- check correct t'
                             , TypedReg t fr) -- TODO Check this is correct 't'

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

                pure ( prs ++ trs ++ fls ++ [ILabel dnLabel]
                     , TypedReg (typeOf tr) fr )

            CCase _ (ATerm _ term) patterns ->

                case term of

                    LitInt i -> do
                        let todoInstrs = [ comment deps ("{case of litint " <> (C8.pack $ show i) <> "}")
                                         , comment deps "{patterns}" ]
                        pure (todoInstrs, VInt 37)

                    Var v -> do
                        let todoInstrs = [ comment deps ("{case of var " <> sToBs deps v <> "}")
                                         , comment deps "{patterns}" ]
                        pure (todoInstrs, VInt 37)

                    DCons dc -> do
                        let todoInstrs = [ comment deps ("{case of datacons " <> sToBs deps dc <> "}")
                                         , comment deps "{patterns}" ]
                        pure (todoInstrs, VInt 38)

assignToStruct :: (Eq s, Show s) => Deps s
                                 -> Val s
                                 -> EitherT ByteString (State (GenState s)) (Val s, [Instr s])
assignToStruct deps c@(VDConsTyped t name tag xs) = do

    let sz = getSize (SizedVal c)

    fr <- genFresh deps FrReg

    let destOffsets = init
                    . scanl (+) 0
                    $ 8 : map (getSize . SizedVal) xs

    -- fr points to the start of malloc.  destOffsets relative to it

    let instrs = map (toInstr fr)
               . zip destOffsets
               $ VTag tag:xs

    let rType = typeApply t (map (getTypeOfVal deps) xs)

    pure ( TypedReg rType fr
         , Malloc fr sz : instrs )

    where
    toInstr fr (o, v) = Cpy (RegPtrOff fr o) v

typeApply :: (Eq s, Show s) => Type s -> [Type s] -> Type s
typeApply t' ts' = go t' ts'
    where
    go           t     []          = t
    go (TyArr a b) (t:ts) | a == t = typeApply b ts
    go           _      _          = error $ "Couldn't apply types: (" ++ show t' ++ ") (" ++ show ts' ++ ")"

goTerm :: (Ord s, Show s) => Type s
                          -> Term s
                          -> EitherT ByteString (State (GenState s)) (Val s)
goTerm typ term =

    case term of

        Var v -> do
            mr <- M.lookup v . regMap <$> lift get
            pure $ case mr of
                       Just r  -> TypedReg typ r
                       Nothing -> Label v

        LitInt i ->
            pure $ VInt i

        LitBool b ->
            pure $ VBool b

        DCons dc ->
            pure $ VDConsNameTyped typ dc
