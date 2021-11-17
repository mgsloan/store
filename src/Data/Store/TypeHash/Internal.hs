{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Store.TypeHash.Internal where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Monad (when, unless)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import           Data.Char (isUpper, isLower)
import           Data.Data (Data)
import           Data.Functor.Contravariant
import           Data.Generics (listify)
import           Data.List (sortBy)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import           Data.Proxy (Proxy(..))
import           Data.Store
import           Data.Store.Internal
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany (reifyMany)
import           Language.Haskell.TH.Syntax (Lift(..), unsafeTExpCoerce)
import           Prelude

{-# DEPRECATED mkManyHasTypeHash, mkHasTypeHash
    "Use of Data.Store.TypeHash isn't recommended, as the hashes are too unstable for most uses.  Please instead consider using Data.Store.Version.  See https://github.com/fpco/store/issues/53"
  #-}

newtype Tagged a = Tagged { unTagged :: a }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData a => NFData (Tagged a)

instance (Store a, HasTypeHash a) => Store (Tagged a) where
    size = addSize 20 (contramap unTagged size)
    peek = do
        tag <- peek
        let expected = typeHash (Proxy :: Proxy a)
        when (tag /= expected) $ fail "Mismatched type hash"
        Tagged <$> peek
    poke (Tagged x) = do
        poke (typeHash (Proxy :: Proxy a))
        poke x

newtype TypeHash = TypeHash { unTypeHash :: StaticSize 20 BS.ByteString }
    deriving (Eq, Ord, Show, Store, Generic)

#if __GLASGOW_HASKELL__ >= 710
deriving instance Typeable TypeHash
deriving instance Data TypeHash
#endif

instance NFData TypeHash

instance Lift TypeHash where
    lift = staticByteStringExp . unStaticSize . unTypeHash
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = Code . unsafeTExpCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif

reifyManyTyDecls :: ((Name, Info) -> Q (Bool, [Name]))
                 -> [Name]
                 -> Q [(Name, Info)]
reifyManyTyDecls f = reifyMany go
  where
    go x@(_, TyConI{}) = f x
    go x@(_, FamilyI{}) = f x
    go x@(_, PrimTyConI{}) = f x
    go x@(_, DataConI{}) = f x
    go (_, ClassI{}) = return (False, [])
    go (_, ClassOpI{}) = return (False, [])
    go (_, VarI{}) = return (False, [])
    go (_, TyVarI{}) = return (False, [])
#if MIN_VERSION_template_haskell(2,12,0)
    go (_, PatSynI{}) = return (False, [])
#endif

-- | At compiletime, this yields a hash of the specified datatypes.
-- Not only does this cover the datatypes themselves, but also all
-- transitive dependencies.
--
-- The resulting expression is a literal of type 'TypeHash'.
typeHashForNames :: [Name] -> Q Exp
typeHashForNames ns = do
    infos <- getTypeInfosRecursively ns
    [| TypeHash $(staticByteStringExp (SHA1.hash (encode infos))) |]

-- | At compiletime, this yields a cryptographic hash of the specified 'Type',
-- including the definition of things it references (transitively).
--
-- The resulting expression is a literal of type 'TypeHash'.
hashOfType :: Type -> Q Exp
hashOfType ty = do
    unless (null (getVarNames ty)) $ fail $ "hashOfType cannot handle polymorphic type " <> pprint ty
    infos <- getTypeInfosRecursively (getConNames ty)
    [| TypeHash $(staticByteStringExp (SHA1.hash (encode infos))) |]

getTypeInfosRecursively :: [Name] -> Q [(Name, Info)]
getTypeInfosRecursively names = do
    allInfos <- reifyManyTyDecls (\(_, info) -> return (True, getConNames info)) names
    -- Sorting step probably unnecessary because this should be
    -- deterministic, but hey why not.
    return (sortBy (comparing fst) allInfos)

getConNames :: Data a => a -> [Name]
getConNames = listify (isUpper . head . nameBase)

getVarNames :: Data a => a -> [Name]
getVarNames = listify (isLower . head . nameBase)

-- TODO: Generic instance for polymorphic types, or have TH generate
-- polymorphic instances.

class HasTypeHash a where
    typeHash :: Proxy a -> TypeHash

mkHasTypeHash :: Type -> Q [Dec]
mkHasTypeHash ty =
    [d| instance HasTypeHash $(return ty) where
            typeHash _ = $(hashOfType ty)
      |]

mkManyHasTypeHash :: [Q Type] -> Q [Dec]
mkManyHasTypeHash qtys = concat <$> mapM (mkHasTypeHash =<<) qtys

combineTypeHashes :: [TypeHash] -> TypeHash
combineTypeHashes = TypeHash . toStaticSizeEx . SHA1.hash . BS.concat . map (unStaticSize . unTypeHash)
