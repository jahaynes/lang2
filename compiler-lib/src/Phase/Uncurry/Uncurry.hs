module Phase.Uncurry.Uncurry where

import Core.Types
import Phase.Anf.AnfExpression
import Phase.Anf.AnfModule

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

uncurryModule :: (Ord s, Show s) => AnfModule s -> Either ByteString (AnfModule s)
uncurryModule anfM = do 

    let fundefns = map findTypes
                 $ getFunDefAnfTs anfM
    Left . C8.unlines . map C8.pack . map show $ fundefns
    -- TODO inspect all call sites and try to match up parameters

findTypes :: Eq s => FunDefAnfT s -> (s, [Type s], Type s)
findTypes (FunDefAnfT name _ fundef) =

    let returnType = findReturnType fundef
        inputTypes = findInputTypesFromReturnType returnType (typeOf fundef) 

    in (name, inputTypes, returnType)

    where
    findReturnType :: NExp s -> Type s
    findReturnType def =

        case def of
            AExp (ALam _ _ body) -> goNexp body
            _                    -> goNexp def

        where
        goNexp (NLet _ _ c) = goNexp c
        goNexp (CExp cexp)  = goCexp cexp
        goNexp (AExp aexp)  = goAexp aexp

        goCexp (CApp t _ _) = t

        goAexp (ABinPrimOp t _ _ _) = t
        goAexp (ATerm t _)          = t
        goAexp (ALam t _ _)         = t

    findInputTypesFromReturnType :: Eq s => Type s -> Type s -> [Type s]
    findInputTypesFromReturnType returnType = go []
        where
        go acc (TyArr a b)
            | b == returnType = reverse (a:acc)
            | otherwise       = go (a:acc) b
        go [] _ = []