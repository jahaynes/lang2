{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Phase.CodeGen.CodeGenA (AInstr, codeGenModuleA, renderCodeGenA) where

import Common.EitherT          (EitherT (..), left)
import Common.ReaderT          (ReaderT (..), ask)
import Common.State
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AExp (..), CExp (..), NExp (..), PExp (..), PPat (..), typeOf, typeOfAExp)
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.SizeInfo
import Phase.CodeGen.TagInfo
import Phase.CodeGen.TypesA
import TypeSystem.Common       (Subst (..))

import           Control.Monad               (forM)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char     (isSpace)
import           Data.Functor  ((<&>))
import           Data.Map      ((!), Map)
import qualified Data.Map as M

type Cg a =
    EitherT ByteString (
        ReaderT [DataDefn ByteString] (
            State (Gen ByteString))) a

data Gen s =
    Gen { regCount     :: !Int
        , varRegisters :: !(Map s SVal) 
        }

renderCodeGenA :: [AInstr ByteString]
               -> ByteString
renderCodeGenA = C8.dropWhile isSpace . C8.unlines . map go
    where
    go (ALabel s)   = "\n" <> s <> ":"
    go (AComment s) = "  // " <> s

    go (AMov dst src) = "  " <> go' dst <> " <- " <> go' src
    go (AMovToPtrOff   ptr@VirtRegPtr{}  offbytes src@VirtRegPrim{}) = "  " <> go' ptr <> "[" <> go'' offbytes <> "]" <> " <- " <> go' src 
    go (AMovFromPtrOff dst@VirtRegPrim{} offbytes src@VirtRegPtr{})  = "  " <> go' dst <> " <- " <> go' src <> "[" <> go'' offbytes <> "]"
    go (ABinOp dst op a b) = "  " <> go' dst <> " <- " <> go'' op <> " " <> go' a <> " " <> go' b
    go (ACmpB a) = "  cmpb " <> go' a

    go (Allocate dst sz) = "  " <> go' dst <> " <- alloc " <> C8.pack (show sz)

    go (Push dbgName t val) = "  push " <> go' val <> " :: " <> go'' t <> " // '" <> dbgName <> "'"
    go (Pop  dbgName t val) = "  " <> go' val <> " <- pop :: " <> go'' t <> " // '" <> dbgName <> "'" 
    go (Call f)   = "  call " <> f
    go (Ret r)    = "  ret " <> go' r
    go (J lbl)    = "  j " <> lbl
    go (Je lbl)   = "  je " <> lbl
    go (Jne lbl)  = "  jne " <> lbl
    go (AErr msg) = "  err \"" <> msg <> "\""
    go x          = "  renderCodeGenA.go: " <> C8.pack (show x)

    go' (VirtRegPrim n) = "vr_"  <> C8.pack (show n)
    go' (VirtRegPtr n)  = "vrp_" <> C8.pack (show n)
    go' (RLitBool b)    = C8.pack (show b)
    go' (RLitInt i)     = C8.pack (show i)

    go'' t = C8.pack (show t)

codeGenModuleA :: AnfModule ByteString
               -> Either ByteString [AInstr ByteString]
codeGenModuleA modu =
    let xs = concat <$> mapM codeGenFunDefn (getFunDefAnfTs modu)
        -- dConsTypesMap = loadDConsTypesMap (getDataDefnAnfTs modu)
    in evalState (runReaderT (runEitherT xs) (getDataDefnAnfTs modu)) (Gen 0 mempty)

codeGenFunDefn :: FunDefAnfT ByteString
               -> Cg [AInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) =
    codeGenNexp nexp <&> \(r, nexp') ->
        concat [ [ALabel name]
               , nexp'
               , [Ret r] ]

codeGenNexp :: NExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenNexp (AExp aexp)  = codeGenAexp aexp
codeGenNexp (CExp cexp)  = codeGenCexp cexp
codeGenNexp (NLet a b c) = codeGenNlet a b c

codeGenAexp :: AExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenAexp (ATerm _type term)          = codeGenTerm term
codeGenAexp (ALam t vs nexp)            = codegenLam t vs nexp
codeGenAexp (AClo _type _fvs _vs _nexp) = pure (unkn, [AComment "closure"])

codeGenAexp (ABinPrimOp t op a b) = do
    (areg, aInstrs) <- codeGenAexp a
    (breg, bInstrs) <- codeGenAexp b
    dest <- freshRegisterFor t
    let instrs = concat [ aInstrs
                        , bInstrs
                        , [ABinOp dest op areg breg] ]
    pure (dest, instrs)

codeGenAexp aexp = pure (unkn, [AComment $ "unknown aexp: " <> C8.pack (show aexp)])

codegenLam :: Type ByteString
           -> [ByteString]
           -> NExp ByteString
           -> Cg (SVal, [AInstr ByteString])
codegenLam t vs nexp = do

    regMap0 <- saveRegisterMap

    pops <- popsForwardOrder t vs
    (ret, nexp') <- codeGenNexp nexp
    let instrs = concat [ pops
                        , nexp' ]
    restoreRegisterMap regMap0
    pure (ret, instrs)

codeGenNlet :: ByteString
            -> NExp ByteString
            -> NExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenNlet a b c = do

    fresh <- freshRegisterFor (typeOf b)
    register a fresh

    (breg, bInstrs) <- codeGenNexp b
    (creg, cInstrs) <- codeGenNexp c

    let instrs = concat [ bInstrs
                        , [AMov fresh breg]
                        , cInstrs ]

    pure (creg, instrs)

bind :: Type s -> [a] -> [(Type s, a)]
bind (TyArr a b) (v:vs) = (a,v) : bind b vs
bind           _     [] = []

popsForwardOrder :: Type ByteString
                 -> [ByteString]
                 -> Cg [AInstr ByteString]
popsForwardOrder ty = mapM go . bind ty
    where
    go (t, v) = do
        fresh <- freshRegisterFor t
        register v fresh
        pure $ Pop v t fresh

-- todo fold
pushesReverseOrder :: [(AExp ByteString, SVal)] -> [AInstr ByteString]
pushesReverseOrder = go []
    where
    go acc               [] = acc
    go acc ((aexp, val):xs) = go (Push (describe aexp) (typeOfAExp aexp) val:acc) xs
        where
        describe (ABinPrimOp _ AddI _ _) = "+"
        describe (ABinPrimOp _ SubI _ _) = "-"
        describe (ATerm _ (Var v)) = v
        describe (ATerm _ (LitInt i)) = C8.pack (show i)
        describe x = "Not described: " <> C8.pack (show x)

codeGenCexp :: CExp ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenCexp (CApp t f xs) = codeGenApp t f xs
codeGenCexp (CAppClo t f cloEnv xs) = codeGenAppClo t f cloEnv xs
codeGenCexp (CIfThenElse t pr tr fl) = codeGenIfThenElse t pr tr fl
codeGenCexp (CCase t scrut ps) = codeGenCase t scrut ps
codeGenCexp cexp = left $ "codeGenCexp: " <> C8.pack (show cexp)

codeGenApp :: Type ByteString
           -> AExp ByteString
           -> [AExp ByteString]
           -> Cg (SVal, [AInstr ByteString])

codeGenApp t (ATerm _ (DCons dc)) xs = do

    Tag n <- getTag t dc

    (argsInstr, argsRegs) <-
        mapM codeGenAexp xs <&> \xs' ->
            (concatMap snd xs', map fst xs')

    rr <- freshRegisterFor t

    allocLayout <- sizeOfDConsInstance dc t
    let sz    = totalSz allocLayout
        strSz = C8.pack $ show sz

    let instrs = concat [ [AComment "Evaluating args"]
                        , argsInstr
                        , [AComment $ "Want to allocate: " <> strSz <> " for " <> dc]
                        , [Allocate rr sz]
                        , [AComment $ "Want to store the tag at 0 (if applicable)"]
                        , [AMovToPtrOff rr 0 (RLitInt $ fromIntegral n)]
                        , [AComment $ "Want to store into offsets: " <> C8.pack (show $ fieldOffsets allocLayout)]
                        , zipWith (AMovToPtrOff rr) (fieldOffsets allocLayout) argsRegs
                        , [AComment "Done"] ]

    pure (rr, instrs)

codeGenApp t (ATerm _ (Var v)) xs = do

    -- Get the args ready to be pushed
    (xs', prePushInstrs) <- unzip <$> mapM codeGenAexp xs
    let pushes = pushesReverseOrder (zip xs xs')

    rr <- freshRegisterFor t

    let instrs = concat [ concat prePushInstrs
                        , pushes
                        , [ Call v
                          , Pop ("ret from " <> v) t rr ] ]

    pure (rr, instrs)

-- TODO
codeGenAppClo _t _f _cloEnv _xs = do
    pure (unkn, [ALabel "..codeGenAppClo.."])

codeGenIfThenElse t pr tr fl = do
    rr                <- freshRegisterFor t
    branchLables      <- mapM freshBranchLabel ["if_", "then_", "else_", "endif_"]
    (prReg, prInstrs) <- codeGenAexp pr
    (trReg, trInstrs) <- codeGenNexp tr
    (flReg, flInstrs) <- codeGenNexp fl

    let [if_, then_, else_, endif_] = branchLables

    let instrs = concat [ [ALabel if_]
                        , prInstrs
                        , [ ACmpB prReg
                          , Jne else_ ]
                        , [ALabel then_]
                        , trInstrs
                        , [ AMov rr trReg
                          , J endif_ ]
                        , [ALabel else_]
                        , flInstrs
                        , [ AMov rr flReg
                          , ALabel endif_] ]

    pure (rr, instrs)

codeGenCase :: Type ByteString
            -> AExp ByteString
            -> [PExp ByteString]
            -> Cg (SVal, [AInstr ByteString])
codeGenCase t scrut ps = do

    lhsTypeMap <- instantiateLhs (typeOfAExp scrut)

    -- Return to this reg
    destReg <- freshRegisterFor t

    -- Handle the scrutinee
    (scrutReg, scrutInstrs) <- codeGenAexp scrut

    -- Prepare a register to hold the tag
    tag <- freshRegisterFor (TyCon "Int" [])

    -- A register for comparing the tag
    sharedCmp <- freshRegisterFor (TyCon "Bool" [])

    -- Prepare a 'done' label for all branches to jump to
    doneLabel <- freshBranchLabel "done_"

    checksAndLoads <- forM ps $ \(PExp lhs rhs) -> do

        -- Get the tag num to check against
        let PApp dc _ _ = lhs
            Just (i, dctypes) = M.lookup dc lhsTypeMap

        -- Make a label for this pattern
        label <- freshBranchLabel ("pat_" <> C8.pack (show i) <> "_" <> dc <> "_")

                          -- write the comparison bool to sharedCmp
        let checkInstrs = [ ABinOp sharedCmp EqA tag (RLitInt $ fromIntegral i)

                          -- Check the comparison
                          , ACmpB sharedCmp

                          -- Conditional jmp to label
                          , Je label ]

        regMap0  <- saveRegisterMap
        lhsLoads <- codeGenPattern dctypes label scrutReg lhs rhs destReg doneLabel
        restoreRegisterMap regMap0
        pure (checkInstrs, lhsLoads)

    let (checks, loads) = unzip checksAndLoads

    -- Prepare an 'inexhause' label if the scrutinee doesn't match any of the patterns
    inexhaust_ <- freshBranchLabel "inexhaust_"

    let instrs = concat [ scrutInstrs
                        , [ AComment "Checking constructor tag"
                          , AMovFromPtrOff tag 0 scrutReg ]
                        , concat checks
                        , [ J inexhaust_ ]
                        , concat loads
                        , [ ALabel inexhaust_
                          , AErr "inexhaust" ]
                        , [ ALabel doneLabel ]
                        ]

    pure (destReg, instrs)

-- TODO: Assumes Constructor.  No literals yet?
codeGenPattern :: [Type ByteString]
               -> ByteString
               -> SVal
               -> PPat ByteString
               -> NExp ByteString
               -> SVal
               -> ByteString
               -> Cg [AInstr ByteString]
codeGenPattern dctypes label scrutReg (PApp dc dct ms) rhs destReg doneLabel = do

    -- Get the offsets of each pattern's field
    offsets <- fieldOffsets <$> sizeOfDConsInstance dc dct

    lhsInstrs <- forM (zip3 dctypes ms offsets) $ \case
        (t, Var v, off) -> do
            fresh <- freshRegisterFor t
            register v fresh
            pure $ AMovFromPtrOff fresh off scrutReg

    -- Generate code for the RHS
    (rhsReg, rhsInstrs) <- codeGenNexp rhs

    pure $  (ALabel label)
         :  lhsInstrs
         ++ rhsInstrs
         ++ [ AMov destReg rhsReg
            , J doneLabel ]

codeGenTerm :: Term ByteString
            -> Cg (SVal, [AInstr ByteString])
codeGenTerm (LitBool b) = pure (RLitBool b, [])
codeGenTerm (LitInt i)  = pure (RLitInt i, [])
codeGenTerm (Var v) =
    getRegister v >>= \case
        Just r  -> pure (r, [])
        Nothing -> left $ "Unregistered: " <> v -- pure (unkn, []).  Unapplied functions can get snagged here
codeGenTerm term = left $ "codeGenTerm: " <> C8.pack (show term)

getRegister :: ByteString -> Cg (Maybe SVal)
getRegister v = lift $ do
    vr <- varRegisters <$> lift get
    pure $ M.lookup v vr

freshRegisterFor :: Type ByteString -> Cg SVal
freshRegisterFor t =
    freshNum <&> \rc ->
        case t of
            TyCon "Int"  [] -> VirtRegPrim rc
            TyCon "Bool" [] -> VirtRegPrim rc
            _               -> VirtRegPtr rc

freshBranchLabel :: ByteString -> Cg ByteString
freshBranchLabel pre = freshNum <&> \n -> pre <> C8.pack (show n)

freshNum :: Cg Int
freshNum = lift . lift $ do
    gen <- get
    let rc = regCount gen
    put gen { regCount = rc + 1 }
    pure rc

unkn :: SVal
unkn = VirtRegPtr 99

register :: ByteString -> SVal -> Cg ()
register var reg = lift . lift . modify' $ \gen -> gen { varRegisters = M.insert var reg (varRegisters gen) }

saveRegisterMap :: Cg (Map ByteString SVal)
saveRegisterMap = lift $ lift (varRegisters <$> get)

restoreRegisterMap :: Map ByteString SVal -> Cg ()
restoreRegisterMap regMap = lift . lift . modify' $ \gen -> gen { varRegisters = regMap }

-- For each data constructor, return its tag num and the concrete types of its members
instantiateLhs :: Type ByteString -> Cg (Map ByteString (Int, [Type ByteString]))
instantiateLhs (TyCon tc typeInsts) = do

    -- Lookup the right set of data constructors and their type variables
    DataDefn _ tyVars dcons <- head -- TODO error handle
                             . filter (\(DataDefn t _ _) -> t == tc)
                           <$> lift ask

    -- Prepare the variable-to-concrete-type map
    let subst = Subst . M.fromList $ zip tyVars typeInsts

    -- Instantiate all the data constructors
    let dcons' = map (apply subst) dcons

    pure . M.fromList $ zipWith (\(dc, ts) i -> (dc, (i, ts))) dcons' [0..]

    where
    apply :: forall s. (Ord s, Show s) => Subst s -> DataCon s -> (s, [Type s])
    apply (Subst subst) (DataCon n ms) = (n, map go ms)
        where
        go :: Member s -> Type s
        go (MemberType n' tv) = TyCon n' (map go tv)
        go (MemberVar v)      = subst ! v
