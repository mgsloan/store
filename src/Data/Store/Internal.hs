{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Internal API for the store package. The functions here which are
-- not re-exported by "Data.Store" are less likely to have stable APIs.
--
-- This module also defines most of the included 'Store' instances, for
-- types from the base package and other commonly used packages
-- (bytestring, containers, text, time, etc).
module Data.Store.Internal
    (
    -- * Encoding and decoding strict ByteStrings.
      encode,
      decode, decodeWith,
      decodeEx, decodeExWith, decodeExPortionWith
    , decodeIO, decodeIOWith, decodeIOPortionWith
    -- * Store class and related types.
    , Store(..), Poke, Peek, runPeek
    -- ** Exceptions thrown by Poke
    , PokeException(..), pokeException
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException, tooManyBytes
    -- ** Size type
    , Size(..)
    , getSize, getSizeWith
    , combineSize, combineSizeWith, addSize
    -- ** Store instances in terms of IsSequence
    , sizeSequence, pokeSequence, peekSequence
    -- ** Store instances in terms of IsSet
    , sizeSet, pokeSet, peekSet
    -- ** Store instances in terms of IsMap
    , sizeMap, pokeMap, peekMap
    -- *** Utilities for ordered maps
    , sizeOrdMap, pokeOrdMap, peekOrdMapWith
    -- ** Store instances in terms of IArray
    , sizeArray, pokeArray, peekArray
    -- ** Store instances in terms of Generic
    , GStoreSize, genericSize
    , GStorePoke, genericPoke
    , GStorePeek, genericPeek
    -- ** Peek utilities
    , skip, isolate
    , peekMagic
    -- ** Static Size type
    --
    -- This portion of the library is still work-in-progress.
    -- 'IsStaticSize' is only supported for strict ByteStrings, in order
    -- to support the use case of 'Tagged'.
    , IsStaticSize(..), StaticSize(..), toStaticSizeEx, liftStaticSize, staticByteStringExp
    ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Containers (IsMap, ContainerKey, MapValue, mapFromList, mapToList, IsSet, setFromList)
import           Data.Complex (Complex (..))
import           Data.Data (Data)
import           Data.Fixed (Fixed (..), Pico)
import           Data.Foldable (forM_, foldl')
import           Data.Functor.Contravariant
import           Data.Functor.Identity (Identity (..))
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable)
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.MonoTraversable
import           Data.Monoid
import           Data.Orphans ()
import           Data.Primitive.ByteArray
import           Data.Proxy (Proxy(..))
import           Data.Sequence (Seq)
import           Data.Sequences (IsSequence, Index, replicateM)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Impl
import           Data.Store.Core
import           Data.Store.TH.Internal
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Foreign as T
import qualified Data.Text.Internal as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.TAI as Time
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
#if MIN_VERSION_vector(0,13,2)
import qualified Data.Vector.Strict as SCV
import qualified Data.Vector.Strict.Mutable as MSCV
#endif
import           Data.Void
import           Data.Word
import           Foreign.C.Types ()
import           Foreign.Ptr (plusPtr, minusPtr)
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Generics (Generic)
import           GHC.Real (Ratio(..))
import           GHC.TypeLits
import           Instances.TH.Lift ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.ReifyMany
import           Language.Haskell.TH.Syntax
import           Network.Socket (AddrInfo)
import           Numeric.Natural (Natural)
import           Prelude
import           TH.Derive

#if MIN_VERSION_time(1,8,0)
import qualified Data.Time.Clock.System as Time
#endif
#if MIN_VERSION_time(1,9,0)
import qualified Data.Time.Format.ISO8601 as Time
#endif
#if MIN_VERSION_time(1,11,0)
import qualified Data.Time.Calendar.Quarter as Time
import qualified Data.Time.Calendar.WeekDate as Time
#endif

#ifdef INTEGER_GMP
import qualified GHC.Integer.GMP.Internals as I
import           GHC.Types (Int (I#))
#else
import           GHC.Types (Word (W#))
import qualified GHC.Integer.Simple.Internals as I
#endif

-- Conditional import to avoid warning
#ifdef INTEGER_GMP
#if MIN_VERSION_integer_gmp(1,0,0)
import           GHC.Prim (sizeofByteArray#)
#endif
#endif

-- TODO: higher arities?  Limited now by Generics instances for tuples
$(return $ map deriveTupleStoreInstance [2..7])

$(deriveManyStoreFromStorable
  (\ty ->
    case ty of
      ConT n | elem n [''Char, ''Int, ''Int64, ''Word, ''Word8, ''Word32] -> True
      _ -> False
    ))

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'IsSequence'

-- | Implement 'size' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
sizeSequence :: forall t. (IsSequence t, Store (Element t)) => Size t
sizeSequence = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * (olength t) + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSequence #-}

-- | Implement 'poke' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
pokeSequence :: (IsSequence t, Store (Element t)) => t -> Poke ()
pokeSequence t =
  do pokeStorable len
     Poke (\ptr offset ->
             do offset' <-
                  ofoldlM (\offset' a ->
                             do (offset'',_) <- runPoke (poke a) ptr offset'
                                return offset'')
                          offset
                          t
                return (offset',()))
  where len = olength t
{-# INLINE pokeSequence #-}

-- | Implement 'peek' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
peekSequence :: (IsSequence t, Store (Element t), Index t ~ Int) => Peek t
peekSequence = do
    len <- peek
    replicateM len peek
{-# INLINE peekSequence #-}

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'IsSet'

-- | Implement 'size' for an 'IsSet' of 'Store' instances.
sizeSet :: forall t. (IsSet t, Store (Element t)) => Size t
sizeSet = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * (olength t) + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSet #-}

-- | Implement 'poke' for an 'IsSequence' of 'Store' instances.
pokeSet :: (IsSet t, Store (Element t)) => t -> Poke ()
pokeSet t = do
    pokeStorable (olength t)
    omapM_ poke t
{-# INLINE pokeSet #-}

-- | Implement 'peek' for an 'IsSequence' of 'Store' instances.
peekSet :: (IsSet t, Store (Element t)) => Peek t
peekSet = do
    len <- peek
    setFromList <$> replicateM len peek
{-# INLINE peekSet #-}

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of a 'IsMap'

-- | Implement 'size' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
sizeMap
    :: forall t. (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Size t
sizeMap = VarSize $ \t ->
    case (size :: Size (ContainerKey t), size :: Size (MapValue t)) of
        (ConstSize nk, ConstSize na) -> (nk + na) * olength t + sizeOf (undefined :: Int)
        (szk, sza) -> ofoldl' (\acc (k, a) -> acc + getSizeWith szk k + getSizeWith sza a)
                              (sizeOf (undefined :: Int))
                              (mapToList t)
{-# INLINE sizeMap #-}

-- | Implement 'poke' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
pokeMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => t
    -> Poke ()
pokeMap = pokeSequence . mapToList
{-# INLINE pokeMap #-}

-- | Implement 'peek' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
peekMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Peek t
peekMap = mapFromList <$> peek
{-# INLINE peekMap #-}

------------------------------------------------------------------------
-- Utilities for defining 'Store' instances for ordered containers like
-- 'IntMap' and 'Map'

-- | Marker for maps that are encoded in ascending order instead of the
-- descending order mistakenly implemented in 'peekMap' in store versions
-- < 0.4.
--
-- See https://github.com/fpco/store/issues/97.
markMapPokedInAscendingOrder :: Word32
markMapPokedInAscendingOrder = 1217678090

-- | Ensure the presence of a given magic value.
--
-- Throws a 'PeekException' if the value isn't present.
peekMagic
    :: (Eq a, Show a, Store a)
    => String -> a -> Peek ()
peekMagic markedThing x = do
    x' <- peek
    when (x' /= x) $
        fail ("Expected marker for " ++ markedThing ++ ": " ++ show x ++ " but got: " ++ show x')
{-# INLINE peekMagic #-}

-- | Like 'sizeMap' but should only be used for ordered containers where
-- 'Data.Containers.mapToList' returns an ascending list.
sizeOrdMap
    :: forall t.
       (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Size t
sizeOrdMap =
    combineSizeWith (const markMapPokedInAscendingOrder) id size sizeMap
{-# INLINE sizeOrdMap #-}

-- | Like 'pokeMap' but should only be used for ordered containers where
-- 'Data.Containers.mapToList' returns an ascending list.
pokeOrdMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => t -> Poke ()
pokeOrdMap x = poke markMapPokedInAscendingOrder >> pokeMap x
{-# INLINE pokeOrdMap #-}

-- | Decode the results of 'pokeOrdMap' using a given function to construct
-- the map.
peekOrdMapWith
    :: (Store (ContainerKey t), Store (MapValue t))
    => ([(ContainerKey t, MapValue t)] -> t)
       -- ^ A function to construct the map from an ascending list such as
       -- 'Map.fromDistinctAscList'.
    -> Peek t
peekOrdMapWith f = do
    peekMagic "ascending Map / IntMap" markMapPokedInAscendingOrder
    f <$> peek
{-# INLINE peekOrdMapWith #-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances for list-like mutable things

-- | Implementation of peek for mutable sequences. The user provides a
-- function for initializing the sequence and a function for mutating an
-- element at a particular index.
peekMutableSequence
    :: Store a
    => (Int -> IO r)
    -> (r -> Int -> a -> IO ())
    -> Peek r
peekMutableSequence new write = do
    n <- peek
    mut <- liftIO (new n)
    forM_ [0..n-1] $ \i -> peek >>= liftIO . write mut i
    return mut
{-# INLINE peekMutableSequence #-}

------------------------------------------------------------------------
-- Useful combinators

-- | Skip n bytes forward.
{-# INLINE skip #-}
skip :: Int -> Peek ()
skip len = Peek $ \ps ptr -> do
    let ptr2 = ptr `plusPtr` len
        remaining = peekStateEndPtr ps `minusPtr` ptr
    when (len > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
        tooManyBytes len remaining "skip"
    return $ PeekResult ptr2 ()

-- | Isolate the input to n bytes, skipping n bytes forward. Fails if @m@
-- advances the offset beyond the isolated region.
{-# INLINE isolate #-}
isolate :: Int -> Peek a -> Peek a
isolate len m = Peek $ \ps ptr -> do
    let end = peekStateEndPtr ps
        ptr2 = ptr `plusPtr` len
        remaining = end `minusPtr` ptr
    when (len > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
        tooManyBytes len remaining "isolate"
    PeekResult ptr' x <- runPeek m ps ptr
    when (ptr' > end) $
        throwIO $ PeekException (ptr' `minusPtr` end) "Overshot end of isolated bytes"
    return $ PeekResult ptr2 x

------------------------------------------------------------------------
-- Instances for types based on flat representations

instance Store a => Store (V.Vector a) where
    size = sizeSequence
    poke = pokeSequence
    peek = V.unsafeFreeze =<< peekMutableSequence MV.new MV.write

#if MIN_VERSION_vector(0,13,2)
instance Store a => Store (SCV.Vector a) where
    size = sizeSequence
    poke = pokeSequence
    peek = SCV.unsafeFreeze =<< peekMutableSequence MSCV.new MSCV.write
#endif

instance Storable a => Store (SV.Vector a) where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        sizeOf (undefined :: a) * SV.length x
    poke x = do
        let (fptr, len) = SV.unsafeToForeignPtr0 x
        poke len
        pokeFromForeignPtr fptr 0 (sizeOf (undefined :: a) * len)
    peek = do
        len <- peek
        fp <- peekToPlainForeignPtr "Data.Storable.Vector.Vector" (sizeOf (undefined :: a) * len)
        liftIO $ SV.unsafeFreeze (MSV.MVector len fp)

instance Store BS.ByteString where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        BS.length x
    poke x = do
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        poke sourceLength
        pokeFromForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        len <- peek
        fp <- peekToPlainForeignPtr "Data.ByteString.ByteString" len
        return (BS.PS fp 0 len)

#if MIN_VERSION_template_haskell(2,16,0)
-- | Template Haskell Bytes are nearly identical to ByteString, but it
-- can't depend on ByteString.
instance Store Bytes where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        fromIntegral (bytesSize x)
    poke (Bytes sourceFp sourceOffset sourceLength) = do
        poke sourceLength
        pokeFromForeignPtr sourceFp (fromIntegral sourceOffset) (fromIntegral sourceLength)
    peek = do
        len <- peek
        fp <- peekToPlainForeignPtr "Data.ByteString.ByteString" (fromIntegral len)
        return (Bytes fp 0 len)
#endif

instance Store SBS.ShortByteString where
    size = VarSize $ \x ->
         sizeOf (undefined :: Int) +
         SBS.length x
    poke x@(SBS.SBS arr) = do
        let len = SBS.length x
        poke len
        pokeFromByteArray arr 0 len
    peek = do
        len <- peek
        ByteArray array <- peekToByteArray "Data.ByteString.Short.ShortByteString" len
        return (SBS.SBS array)

instance Store LBS.ByteString where
    size = VarSize $ \x ->
         sizeOf (undefined :: Int)  +
         fromIntegral (LBS.length x)
    -- TODO: more efficient implementation that avoids the double copy
    poke = poke . LBS.toStrict
    peek = fmap LBS.fromStrict peek

instance Store T.Text where
#if MIN_VERSION_text(2,0,0)
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        T.lengthWord8 x
    poke x = do
        let !(T.Text (TA.ByteArray array) w8Off w8Len) = x
        poke w8Len
        pokeFromByteArray array w8Off w8Len
    peek = do
        w8Len <- peek
        ByteArray array <- peekToByteArray "Data.Text.Text" w8Len
        return (T.Text (TA.ByteArray array) 0 w8Len)
#else
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        2 * (T.lengthWord16 x)
    poke x = do
        let !(T.Text (TA.Array array) w16Off w16Len) = x
        poke w16Len
        pokeFromByteArray array (2 * w16Off) (2 * w16Len)
    peek = do
        w16Len <- peek
        ByteArray array <- peekToByteArray "Data.Text.Text" (2 * w16Len)
        return (T.Text (TA.Array array) 0 w16Len)
#endif

------------------------------------------------------------------------
-- Known size instances

newtype StaticSize (n :: Nat) a = StaticSize { unStaticSize :: a }
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

instance NFData a => NFData (StaticSize n a)

class KnownNat n => IsStaticSize n a where
    toStaticSize :: a -> Maybe (StaticSize n a)

toStaticSizeEx :: IsStaticSize n a => a -> StaticSize n a
toStaticSizeEx x =
    case toStaticSize x of
        Just r -> r
        Nothing -> error "Failed to assert a static size via toStaticSizeEx"

instance KnownNat n => IsStaticSize n BS.ByteString where
    toStaticSize bs
        | BS.length bs == fromInteger (natVal (Proxy :: Proxy n)) = Just (StaticSize bs)
        | otherwise = Nothing

instance KnownNat n => Store (StaticSize n BS.ByteString) where
    size = ConstSize (fromInteger (natVal (Proxy :: Proxy n)))
    poke (StaticSize x) = do
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        pokeFromForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        let len = fromInteger (natVal (Proxy :: Proxy n))
        fp <- peekToPlainForeignPtr ("StaticSize " ++ show len ++ " Data.ByteString.ByteString") len
        return (StaticSize (BS.PS fp 0 len))

-- NOTE: this could be a 'Lift' instance, but we can't use type holes in
-- TH. Alternatively we'd need a (TypeRep -> Type) function and Typeable
-- constraint.
liftStaticSize :: forall n a. (KnownNat n, Lift a) => TypeQ -> StaticSize n a -> ExpQ
liftStaticSize tyq (StaticSize x) = do
    let numTy = litT $ numTyLit $ natVal (Proxy :: Proxy n)
    [| StaticSize $(lift x) :: StaticSize $(numTy) $(tyq) |]

#if MIN_VERSION_template_haskell(2,17,0)
staticByteStringExp :: Quote m => BS.ByteString -> m Exp
#else
staticByteStringExp :: BS.ByteString -> ExpQ
#endif
staticByteStringExp bs =
    [| StaticSize bs :: StaticSize $(litT (numTyLit (fromIntegral len))) BS.ByteString |]
  where
    len = BS.length bs

------------------------------------------------------------------------
-- containers instances

instance Store a => Store [a] where
    size = sizeSequence
    poke = pokeSequence
    peek = peekSequence

instance Store a => Store (NE.NonEmpty a)

instance Store a => Store (Seq a) where
    size = sizeSequence
    poke = pokeSequence
    peek = peekSequence

instance (Store a, Ord a) => Store (Set a) where
    size =
        VarSize $ \t ->
            sizeOf (undefined :: Int) +
            case size of
                ConstSize n -> n * Set.size t
                VarSize f -> Set.foldl' (\acc a -> acc + f a) 0 t
    poke = pokeSet
    peek = Set.fromDistinctAscList <$> peek

instance Store IntSet where
    size = sizeSet
    poke = pokeSet
    peek = IntSet.fromDistinctAscList <$> peek

instance Store a => Store (IntMap a) where
    size = sizeOrdMap
    poke = pokeOrdMap
    peek = peekOrdMapWith IntMap.fromDistinctAscList

instance (Ord k, Store k, Store a) => Store (Map k a) where
    size =
        VarSize $ \t ->
            sizeOf markMapPokedInAscendingOrder + sizeOf (undefined :: Int) +
            case (size, size) of
                (ConstSize nk, ConstSize na) -> (nk + na) * Map.size t
                (szk, sza) ->
                    Map.foldlWithKey'
                        (\acc k a -> acc + getSizeWith szk k + getSizeWith sza a)
                        0
                        t
    poke = pokeOrdMap
    peek = peekOrdMapWith Map.fromDistinctAscList

instance (Eq k, Hashable k, Store k, Store a) => Store (HashMap k a) where
    size = sizeMap
    poke = pokeMap
    peek = peekMap

instance (Eq a, Hashable a, Store a) => Store (HashSet a) where
    size = sizeSet
    poke = pokeSet
    peek = peekSet

instance (A.Ix i, Store i, Store e) => Store (A.Array i e) where
    size = sizeArray
    poke = pokeArray
    peek = peekArray

instance (A.Ix i, A.IArray A.UArray e, Store i, Store e) => Store (A.UArray i e) where
    size = sizeArray
    poke = pokeArray
    peek = peekArray

sizeArray :: (A.Ix i, A.IArray a e, Store i, Store e) => Size (a i e)
sizeArray = VarSize $ \arr ->
    let bounds = A.bounds arr
    in  getSize bounds +
        case size of
            ConstSize n ->  n * A.rangeSize bounds
            VarSize f -> foldl' (\acc x -> acc + f x) 0 (A.elems arr)
{-# INLINE sizeArray #-}

pokeArray :: (A.Ix i, A.IArray a e, Store i, Store e) => a i e -> Poke ()
pokeArray arr = do
    poke (A.bounds arr)
    forM_ (A.elems arr) poke
{-# INLINE pokeArray #-}

peekArray :: (A.Ix i, A.IArray a e, Store i, Store e) => Peek (a i e)
peekArray = do
    bounds <- peek
    let len = A.rangeSize bounds
    elems <- replicateM len peek
    return (A.listArray bounds elems)
{-# INLINE peekArray #-}

instance Store Integer where
#ifdef INTEGER_GMP
#if MIN_VERSION_integer_gmp(1,0,0)
    size = VarSize $ \ x ->
        sizeOf (undefined :: Word8) + case x of
            I.S# _ -> sizeOf (undefined :: Int)
            I.Jp# (I.BN# arr) -> sizeOf (undefined :: Int) + I# (sizeofByteArray# arr)
            I.Jn# (I.BN# arr) -> sizeOf (undefined :: Int) + I# (sizeofByteArray# arr)
    poke (I.S# x) = poke (0 :: Word8) >> poke (I# x)
    poke (I.Jp# (I.BN# arr)) = do
        let len = I# (sizeofByteArray# arr)
        poke (1 :: Word8)
        poke len
        pokeFromByteArray arr 0 len
    poke (I.Jn# (I.BN# arr)) = do
        let len = I# (sizeofByteArray# arr)
        poke (2 :: Word8)
        poke len
        pokeFromByteArray arr 0 len
    peek = do
        tag <- peek :: Peek Word8
        case tag of
            0 -> fromIntegral <$> (peek :: Peek Int)
            1 -> I.Jp# <$> peekBN
            2 -> I.Jn# <$> peekBN
            _ -> peekException "Invalid Integer tag"
      where
        peekBN = do
          len <- peek :: Peek Int
          ByteArray arr <- peekToByteArray "GHC>Integer" len
          return $ I.BN# arr
#else
    -- May as well put in the extra effort to use the same encoding as
    -- used for the newer integer-gmp.
    size = VarSize $ \ x ->
        sizeOf (undefined :: Word8) + case x of
            I.S# _ -> sizeOf (undefined :: Int)
            I.J# sz _ -> sizeOf (undefined :: Int) + (I# sz) * sizeOf (undefined :: Word)
    poke (I.S# x) = poke (0 :: Word8) >> poke (I# x)
    poke (I.J# sz arr)
        | (I# sz) > 0 = do
            let len = I# sz * sizeOf (undefined :: Word)
            poke (1 :: Word8)
            poke len
            pokeFromByteArray arr 0 len
        | (I# sz) < 0 = do
            let len = negate (I# sz) * sizeOf (undefined :: Word)
            poke (2 :: Word8)
            poke len
            pokeFromByteArray arr 0 len
        | otherwise = do
            poke (0 :: Word8)
            poke (0 :: Int)
    peek = do
        tag <- peek :: Peek Word8
        case tag of
            0 -> fromIntegral <$> (peek :: Peek Int)
            1 -> peekJ False
            2 -> peekJ True
            _ -> peekException "Invalid Integer tag"
      where
        peekJ neg = do
          len <- peek :: Peek Int
          ByteArray arr <- peekToByteArray "GHC>Integer" len
          let (sz0, r) = len `divMod` (sizeOf (undefined :: Word))
              !(I# sz) = if neg then negate sz0 else sz0
          when (r /= 0) (peekException "Buffer size stored for encoded Integer not divisible by Word size (to get limb count).")
          return (I.J# sz arr)
#endif
#else
    -- NOTE: integer-simple uses a different encoding than GMP
    size = VarSize $ \ x ->
        sizeOf (undefined :: Word8) + case x of
            I.Positive ds -> (1 + fromIntegral (numDigits ds)) * sizeOf (undefined :: Word)
            I.Negative ds -> (1 + fromIntegral (numDigits ds)) * sizeOf (undefined :: Word)
            I.Naught -> 0
      where
    poke x = case x of
      I.Naught -> poke (0 :: Word8)
      I.Positive ds -> do
        poke (1 :: Word8)
        poke (numDigits ds)
        pokeDigits ds
      I.Negative ds -> do
        poke (2 :: Word8)
        poke (numDigits ds)
        pokeDigits ds
      where
        pokeDigits I.None = pure ()
        pokeDigits (I.Some d ds) = poke (W# d) *> pokeDigits ds
    peek = do
      tag <- peek :: Peek Word8
      case tag of
        0 -> pure I.Naught
        1 -> do
          len <- peek :: Peek Word
          I.Positive <$> peekDigits len
        2 -> do
          len <- peek :: Peek Word
          I.Negative <$> peekDigits len
        _ -> peekException "Invalid Integer tag"
      where
        peekDigits i
          | i <= 0 = pure I.None
          | otherwise = do
              W# d <- peek
              ds <- peekDigits (i - 1)
              pure $! I.Some d ds

numDigits :: I.Digits -> Word
numDigits = go 0
  where go !acc I.None = acc
        go !acc (I.Some _ ds) = go (acc + 1) ds
#endif

-- Piggybacks off of the Integer instance

instance Store Natural where
  size = contramap fromIntegral (size :: Size Integer)
  poke = poke . toInteger
  peek = do
      x <- peek :: Peek Integer
      if x < 0
          then peekException "Encountered negative integer when expecting a Natural"
          else return $ fromIntegral x

------------------------------------------------------------------------
-- Other instances

-- Manual implementation due to no Generic instance for Ratio. Also due
-- to the instance for Storable erroring when the denominator is 0.
-- Perhaps we should keep the behavior but instead a peekException?
--
-- In that case it should also error on poke.
--
-- I prefer being able to Store these, because they are constructable.

instance Store a => Store (Ratio a) where
    size = combineSize (\(x :% _) -> x) (\(_ :% y) -> y)
    poke (x :% y) = poke (x, y)
    peek = uncurry (:%) <$> peek

-- Similarly, manual implementation due to no Generic instance for
-- Complex and Identity in GHC-7.10 and earlier.

$($(derive [d| instance Deriving (Store (Fixed a)) |]))

instance Store Time.DiffTime where
    size = contramap (realToFrac :: Time.DiffTime -> Pico) size
    poke = poke . (realToFrac :: Time.DiffTime -> Pico)
    peek = (realToFrac :: Pico -> Time.DiffTime) <$> peek

instance Store Time.NominalDiffTime where
    size = contramap (realToFrac :: Time.NominalDiffTime -> Pico) size
    poke = poke . (realToFrac :: Time.NominalDiffTime -> Pico)
    peek = (realToFrac :: Pico -> Time.NominalDiffTime) <$> peek

instance Store ()
instance Store a => Store (Dual a)
instance Store a => Store (Sum a)
instance Store a => Store (Product a)
instance Store a => Store (First a)
instance Store a => Store (Last a)
instance Store a => Store (Maybe a)
instance Store a => Store (Const a b)

#if MIN_VERSION_vector(0,13,2)
deriving newtype instance Store a => Store (UV.DoNotUnboxLazy a)
deriving newtype instance Store a => Store (UV.DoNotUnboxStrict a)
deriving newtype instance Store a => Store (UV.DoNotUnboxNormalForm a)
#endif

------------------------------------------------------------------------
-- Instances generated by TH

$($(derive [d|
    instance Store a => Deriving (Store (Complex a))
    instance Store a => Deriving (Store (Identity a))

    instance Deriving (Store All)
    instance Deriving (Store Any)
    instance Deriving (Store Void)
    instance Deriving (Store Bool)
    instance (Store a, Store b) => Deriving (Store (Either a b))

    instance Deriving (Store Time.AbsoluteTime)
    instance Deriving (Store Time.Day)
    instance Deriving (Store Time.LocalTime)
    instance Deriving (Store Time.TimeOfDay)
    instance Deriving (Store Time.TimeZone)
    instance Deriving (Store Time.UTCTime)
    instance Deriving (Store Time.UniversalTime)
    instance Deriving (Store Time.ZonedTime)
    instance Deriving (Store Time.TimeLocale)

#if MIN_VERSION_time(1,8,0)
    instance Deriving (Store Time.SystemTime)
#endif

#if MIN_VERSION_time(1,9,0)
    instance Deriving (Store Time.CalendarDiffDays)
    instance Deriving (Store Time.CalendarDiffTime)
    instance Deriving (Store Time.FormatExtension)
#endif

#if MIN_VERSION_time(1,11,0)
    instance Deriving (Store Time.DayOfWeek)
    instance Deriving (Store Time.FirstWeekType)
    instance Deriving (Store Time.Quarter)
    instance Deriving (Store Time.QuarterOfYear)
#endif

    |]))

$(deriveManyStorePrimVector)

$(deriveManyStoreUnboxVector)

$(deriveManyStoreFromStorable
  -- TODO: Figure out why on GHC-8.2.1 this internal datatype is visible
  -- in the instances of Storable. Here's a gist of an attempt at
  -- debugging the issue:
  --
  -- https://gist.github.com/mgsloan/a7c416b961015949d3b5674ce053bbf6
  --
  -- The mysterious thing is why this is happening despite not having a
  -- direct import of Data.Text.Encoding.
  (\ty ->
    case ty of
      ConT n | nameModule n == Just "Data.Text.Encoding"
            && nameBase n == "DecoderState" -> False
      ConT n | nameModule n == Just "Data.Text.Encoding"
            && nameBase n == "CodePoint" -> False
      ConT n | nameModule n == Just "Network.Socket.Types"
            && nameBase n == "In6Addr" -> False
      -- AddrInfo's Storable instance is lossy, so avoid having a Store
      -- instance for it.
      ConT n | n == ''AddrInfo -> False
      _ -> True
    ))

$(reifyManyWithoutInstances ''Store [''ModName, ''NameSpace, ''PkgName] (const True) >>=
   mapM (\name -> return (deriveGenericInstance [] (ConT name))))

-- Explicit definition needed because in template-haskell <= 2.9 (GHC
-- 7.8), NameFlavour contains unboxed values, causing generic deriving
-- to fail.
#if !MIN_VERSION_template_haskell(2,10,0)
instance Store NameFlavour where
    size = VarSize $ \x -> getSize (0 :: Word8) + case x of
        NameS -> 0
        NameQ mn -> getSize mn
        NameU i -> getSize (I# i)
        NameL i -> getSize (I# i)
        NameG ns pn mn -> getSize ns + getSize pn + getSize mn
    poke NameS = poke (0 :: Word8)
    poke (NameQ mn) = do
        poke (1 :: Word8)
        poke mn
    poke (NameU i) = do
        poke (2 :: Word8)
        poke (I# i)
    poke (NameL i) = do
        poke (3 :: Word8)
        poke (I# i)
    poke (NameG ns pn mn) = do
        poke (4 :: Word8)
        poke ns
        poke pn
        poke mn
    peek = do
        tag <- peek
        case tag :: Word8 of
            0 -> return NameS
            1 -> NameQ <$> peek
            2 -> do
                !(I# i) <- peek
                return (NameU i)
            3 -> do
                !(I# i) <- peek
                return (NameL i)
            4 -> NameG <$> peek <*> peek <*> peek
            _ -> peekException "Invalid NameFlavour tag"
#endif

$(reifyManyWithoutInstances ''Store [''Info] (const True) >>=
   mapM deriveGenericInstanceFromName)
