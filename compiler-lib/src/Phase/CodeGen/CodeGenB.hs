{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Phase.CodeGen.CodeGenB (AInstr, codeGenModuleB, renderCodeGenB) where

import Common.EitherT          (EitherT (..), left)
import Common.ReaderT          (ReaderT (..), ask)
import Common.State
import Common.Trans
import Core.Module
import Core.Operator
import Core.Term               (Term (..))
import Core.Types
import Phase.Anf.AnfExpression (AClosEnv (..), AExp (..), CExp (..), NExp (..), PExp (..), PPat (..), typeOf, typeOfAExp)
import Phase.Anf.AnfModule     (AnfModule (..), FunDefAnfT (..))
import Phase.CodeGen.SizeInfo
import Phase.CodeGen.TagInfo
import Phase.CodeGen.TypesA
import TypeSystem.Common       (Subst (..))

import           Control.Monad               (forM, replicateM, zipWithM_)
import           Data.ByteString.Char8       (ByteString, pack)
import qualified Data.ByteString.Char8 as C8
import           Data.Char     (isSpace)
import           Data.Functor  ((<&>))
import           Data.List     (foldl')
import           Data.Map      ((!), Map)
import qualified Data.Map as M
import           Data.Maybe    (fromJust)
import           Debug.Trace (trace)

type Cg a =
    EitherT ByteString (
        ReaderT [DataDefn ByteString] (
            State (Gen ByteString))) a

data Gen s =
    Gen { regCount     :: !Int
        , varRegisters :: !(Map s Int) 
        }

renderCodeGenB :: [AInstr ByteString]
               -> ByteString
renderCodeGenB = const "todo"

codeGenModuleB :: AnfModule ByteString
               -> Either ByteString [[AInstr ByteString]]
codeGenModuleB modu = flip evalState initState
                    . flip runReaderT initEnv
                    . runEitherT $ mapM codeGenFunDefn (getFunDefAnfTs modu)
    where
    initState = Gen 0 mempty
    initEnv   = getDataDefnAnfTs modu

codeGenFunDefn :: FunDefAnfT ByteString -> Cg [AInstr ByteString]
codeGenFunDefn (FunDefAnfT name _quant nexp) = do
    fresh <- freshNum
    codeGenNexp fresh nexp <&> \nexp' ->
        concat [ [ALabel name]
               , nexp'
               , [Ret (AReg fresh)] ]

codeGenNexp :: Int
            -> NExp ByteString
            -> Cg [AInstr ByteString]
codeGenNexp rr (AExp aexp)  = codeGenAexp rr aexp
codeGenNexp rr (CExp cexp)  = codeGenCexp rr cexp
codeGenNexp rr (NLet a b c) = codeGenNLet rr a b c

codeGenNLet :: Int
            -> ByteString
            -> NExp ByteString
            -> NExp ByteString
            -> Cg [AInstr ByteString]
codeGenNLet rr a b c = do
    regMap <- saveRegisterMap
    -- Registering ra->a before b allows recursion within b
    ra <- freshNum
    register a ra
    bInstrs <- codeGenNexp ra b
    cInstrs <- codeGenNexp rr c
    restoreRegisterMap regMap
    pure $ concat [ bInstrs
                  , cInstrs ] 

codeGenCexp :: Int
            -> CExp ByteString
            -> Cg [AInstr ByteString]
codeGenCexp rr (CApp t (ATerm _ (Var f)) xs) = codeGenApp rr t f xs
codeGenCexp rr c = error $ "codeGenAexp: " ++ show c

codeGenApp :: Int
           -> Type ByteString
           -> ByteString
           -> [AExp ByteString]
           -> Cg [AInstr ByteString]
-- it's possible to tell from t/xs whether this is fully-applied.  Relevant?
-- apply args and see if return type is primitive or function?
codeGenApp rr t f xs = trace (show (f, xs)) $ do

    {-
        In the sample, this handles both steps:
            regular call to f, returned closure
            closure call to anf_0 regular return?
    -}

    xs' <- forM xs $ \x -> do
               xr <- freshNum
               x' <- codeGenAexp xr x
               pure (xr, x')

    let (xsrs, xsis) = unzip xs'

    let pushes = zipWith (\x r -> Push (describe x) (typeOfAExp x) (AReg r)) xs xsrs

    pure $ concat [ concat xsis
                  , pushes
                  , [ Call f (length pushes) 1 ]
                  , [ Pop ("ret from " <> f) (TyCon "TBD" []) rr ]
                  ]



codeGenAexp :: Int
            -> AExp ByteString
            -> Cg [AInstr ByteString]
codeGenAexp rr (ATerm t term) = codeGenTerm rr t term
codeGenAexp rr (ABinPrimOp t op a b) = codeGenBinOp rr t op a b
codeGenAexp rr (ALam t vs body) = codeGenLam rr t vs body
codeGenAexp rr (AClo t fvs vs body) = codeGenClo rr t fvs vs body
codeGenAexp rr x = error $ "codeGenAexp: " ++ show x

codeGenClo :: Int
           -> Type ByteString
           -> [(Type ByteString, ByteString)]
           -> [ByteString]
           -> NExp ByteString
           -> Cg [AInstr ByteString]
codeGenClo rr t fvs vs body = trace "codeGenClo" $ do
    pure []


codeGenLam rr t vs body = trace "codeGenLam" $ do
    pure []

codeGenBinOp :: Int
             -> Type ByteString
             -> BinOp
             -> AExp ByteString
             -> AExp ByteString
             -> Cg [AInstr ByteString]
codeGenBinOp rr _t op a b = do
    ra      <- freshNum
    aInstrs <- codeGenAexp ra a
    rb      <- freshNum
    bInstrs <- codeGenAexp rb b
    pure $ concat [ aInstrs
                  , bInstrs
                  , [ABinOp rr op (typeOfAExp a) (AReg ra) (typeOfAExp b) (AReg rb) ]]

codeGenTerm :: Int
            -> Type ByteString
            -> Term ByteString
            -> Cg [AInstr ByteString]
codeGenTerm rr t@(TyCon "Int" []) (LitInt i) = pure [AMov t (RegFromLitInt rr i)]
codeGenTerm rr t (Var v) =
    getRegister v >>= \case
        Nothing -> error "No such register"
        Just vr -> pure [AMov t (RegFromReg rr vr)]

freshNum :: Cg Int
freshNum = lift . lift $ do
    gen <- get
    let rc = regCount gen
    put gen { regCount = rc + 1 }
    pure rc

getRegister :: ByteString -> Cg (Maybe Int)
getRegister v = lift $ do
    vr <- varRegisters <$> lift get
    pure $ M.lookup v vr

register :: ByteString -> Int -> Cg ()
register var reg = lift . lift . modify' $ \gen -> gen { varRegisters = M.insert var reg (varRegisters gen) }

saveRegisterMap :: Cg (Map ByteString Int)
saveRegisterMap = lift $ lift (varRegisters <$> get)

restoreRegisterMap :: Map ByteString Int -> Cg ()
restoreRegisterMap regMap = lift . lift . modify' $ \gen -> gen { varRegisters = regMap }

describe (ABinPrimOp _ AddI _ _) = "+"
describe (ABinPrimOp _ SubI _ _) = "-"
describe (ATerm _ (Var v)) = v
describe (ATerm _ (LitInt i)) = C8.pack (show i)
describe x = "Not described: " <> C8.pack (show x)
