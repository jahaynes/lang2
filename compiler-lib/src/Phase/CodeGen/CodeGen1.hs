{-# LANGUAGE OverloadedStrings #-}

module Phase.CodeGen.CodeGen1 (Val (..), codeGenModule1, renderCodeGen1) where

import Common.State
import Core.Operator
import Core.Term
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule (AnfModule (..), FunDefAnfT (..))

import           Data.ByteString.Char8 (ByteString)
import           Data.Map.Strict       (Map)
import qualified Data.Map as M
import           Data.Set              (Set)
import qualified Data.Set as S
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding

newtype StackAddr =
    StackAddr Int
        deriving (Eq, Ord, Show)

newtype TopLevels =
    TopLevels (Set ByteString)

newtype StackInfo =
    StackInfo Int
        deriving (Eq, Ord, Show)

data Val = VFun StackInfo Val
         | VUnOp UnOp Val
         | VBinOp BinOp Val Val
         | VStackValAt ByteString StackAddr
         | VApp Val [Val]
         | VIf Val Val Val
         | VBool Bool
         | VInt Integer
         | VLabel ByteString
         | VLet Val Val Val
             deriving Show

data FunState = 
    FunState { getStackAddrs :: !(Map ByteString StackAddr)
             , getStackNum   :: !StackAddr }

renderCodeGen1 :: [(ByteString, Val)] -> Text
renderCodeGen1 = T.unlines . map render
    where
    render (name, VFun stackInfo body) = mconcat [ decodeUtf8 name
                                                 , " ("
                                                 , T.pack $ show stackInfo
                                                 , "):\n"
                                                 , T.pack $ show body
                                                 , "\n"
                                                 ]

codeGenModule1 :: AnfModule ByteString -> [(ByteString, Val)]
codeGenModule1 modu =
    let funDefs   = getFunDefAnfTs modu
        topLevels = TopLevels . S.fromList $ map (\(FunDefAnfT name _ _) -> name) funDefs
    in map (process topLevels) funDefs

process :: TopLevels -> FunDefAnfT ByteString -> (ByteString, Val)
process tl (FunDefAnfT name q defn) =

    case defn of

        AExp (ALam _ vs body) ->
            (name, asValLam tl vs body)

        -- force everything into lambda?
        expr ->
            process tl (FunDefAnfT name q (AExp (ALam (typeOf expr) [] expr)))

asVal :: TopLevels -> NExp ByteString -> State FunState Val
asVal tl (AExp aexp) = asValA tl aexp
asVal tl (CExp cexp) = asValC tl cexp
asVal tl (NLet a b c) = VLet <$> letAsStackVar
                             <*> asVal tl b
                             <*> asVal tl c

    where
    letAsStackVar :: State FunState Val
    letAsStackVar = do
        st <- get
        let stackAddr = getStackNum st
        put st { getStackAddrs = M.insert a stackAddr (getStackAddrs st)
               , getStackNum   = next stackAddr }
        pure $ VStackValAt a stackAddr

asValLam :: TopLevels -> [ByteString] -> NExp ByteString -> Val
asValLam tl vs body =
    let stackAddrs      = M.fromList $ zip vs (map StackAddr [1..])
        state0          = FunState stackAddrs (StackAddr $ 1 + length vs)
        (body', state1) = runState' state0 (asVal tl body)
        StackAddr sz    = getStackNum state1
        stackInfo       = StackInfo sz
    in VFun stackInfo body' 

asValA :: TopLevels -> AExp ByteString -> State FunState Val
asValA tl aexp =

    case aexp of

        ATerm _ term ->
            asValT tl term

        ALam{} ->
            error "Nested lambdas disallowed"

        AClo{} ->
            error "Allowed here?"

        AUnPrimOp _ op a ->
            VUnOp op <$> asValA tl a

        ABinPrimOp _ op a b ->
            VBinOp op <$> asValA tl a
                      <*> asValA tl b

asValT :: TopLevels -> Term ByteString -> State FunState Val
asValT (TopLevels t) term = do
    stackAddrs <- getStackAddrs <$> get
    pure $ case term of
             LitBool b -> VBool b
             LitInt i -> VInt i
             Var v ->
                case M.lookup v stackAddrs of
                    Just sa -> VStackValAt v sa
                    Nothing ->
                      if S.member v t
                        then VLabel v
                        else error $ "Not in stack or top-level: " ++ show v
             _ -> error $ "unknown term"

asValC :: TopLevels -> CExp ByteString -> State FunState Val
asValC tl cexp =

    case cexp of

        CIfThenElse _ pr tr fl ->
            VIf <$> asValA tl pr
                <*> asVal  tl tr
                <*> asVal  tl fl

        CApp _ f xs ->
            VApp <$>       asValA tl  f
                 <*> mapM (asValA tl) xs

        CCase _ _scrut _ps ->
            error "caseof cexp"

{- END Preflight -}






{-
    prepareFunction (modu, "main", empty, empty)




prepareFunction (modu, funName, evaldArgs, unevaldArgs) =
    case unevaldArgs of
        Empty ->
            let funAddr = lookupFun funName
            in callFunction (modu, funAddr, evaldArgs)
        ua :<| uas ->
            let a = force ua
            in prepareFunction (modu, funName, evaldArgs |> a, uas)

callFunction (modu, funAddr, evaldArgs) =
    pure "foo"

force x = x

lookupFun y = y
-}


class Next a where
    next :: a -> a

instance Next StackAddr where
    next (StackAddr i) = StackAddr $ i + 1
