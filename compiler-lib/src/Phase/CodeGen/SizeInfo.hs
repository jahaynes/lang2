{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Phase.CodeGen.SizeInfo where

import Common.EitherT
import Common.ReaderT
import Common.Trans
import Core.Module
import Core.Types

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map ((!), Map)
import qualified Data.Map as M

sizeOfTag :: Int
sizeOfTag = 8

-- TODO this doesn't have pointers to sub objects yet
-- TODO differentiate between tagged object and non?
data AllocLayout =
    AllocLayout { totalSz      :: !Int
                , tagOffset    :: !(Maybe Int) -- 0 if present.
                , fieldOffsets :: ![Int]
                } deriving Show

    -- | PrimLayout ?

-- TODO assert tvs length == tyVars length
sizeOfDConsInstance :: Monad m
                    => ByteString -> Type ByteString -> EitherT ByteString
                                                            (ReaderT [DataDefn ByteString] m)
                                                                AllocLayout
sizeOfDConsInstance dcName (TyCon n tvs) = do

    dafaDefns <- lift ask

    -- Locate the right type definition
    let [DataDefn _ tyVars dCons] = filter (\(DataDefn typeName _ _) -> typeName == n) dafaDefns

    -- Prepare the substitution
    let subst = M.fromList $ zip tyVars tvs

    -- Locate the right constructor definition
    let [dc] = filter (\(DataCon conName _) -> conName == dcName) dCons

    -- Substitute the type variable for concrete
    concreteTypes <- applySubst subst dc

    allocationSize concreteTypes

applySubst :: (Ord s, Show s, Monad m) => Map s (Type s) -> DataCon s -> EitherT ByteString m [Type s]
applySubst subst (DataCon _ members) = mapM go members
    where
    go (MemberVar t)      = pure $ subst ! t -- TODO missing case?
    go (MemberType tc ts) = TyCon tc <$> mapM go ts

-- TODO: Assumes a tag.
-- TOOD: foldlM ?
allocationSize :: Monad m => [Type ByteString] -> EitherT ByteString m AllocLayout
allocationSize = go sizeOfTag []
    where
    go !sz offs     [] = pure $ AllocLayout sz (Just 0) (reverse offs)
    go  sz offs (t:ts) = sizeOf t >>= \x -> go (sz+x) (sz:offs) ts

sizeOf :: Monad m => Type ByteString -> EitherT ByteString m Int
sizeOf (TyCon "Int"    []) = pure 8
sizeOf (TyCon "Bool"   []) = pure 8
sizeOf (TyCon "String" []) = left "Not handling sizeOf(String) just yet"

sizeOf (TyCon _ _) = -- This is either a ptr or an Int/Bool.  Both are 8
    pure 8

sizeOf (TyVar _) =
    left "Requested sizeOf on type variable.  Should it have been instantiated to a concrete type?"

sizeOf (TyArr _ _) =
    left "Requested sizeOf on type arrow.  Does that make sense?"
