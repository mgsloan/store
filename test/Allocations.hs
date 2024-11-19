{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Weigh Store's operations.

module Main where

import           Control.DeepSeq
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Serialize as Cereal
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Store as Store
import qualified Data.Vector as Boxed
#if MIN_VERSION_vector(0,13,2)
import qualified Data.Vector as BoxedStrict
#endif
import qualified Data.Vector.Serialize ()
import qualified Data.Vector.Storable as Storable
import           Text.Printf
import           Weigh

-- | Main entry point.
main :: IO ()
main =
  mainWith weighing

-- | Weigh weighing with Store vs Cereal.
weighing :: Weigh ()
weighing =
  do fortype "[Int]" (\n -> replicate n 0 :: [Int])
     fortype "Boxed Vector Int" (\n -> Boxed.replicate n 0 :: Boxed.Vector Int)
#if MIN_VERSION_vector(0,13,2)
     fortype "Boxed Strict Vector Int" (\n -> BoxedStrict.replicate n 0 :: BoxedStrict.Vector Int)
#endif
     fortype "Storable Vector Int"
             (\n -> Storable.replicate n 0 :: Storable.Vector Int)
     fortype "Set Int" (Set.fromDistinctAscList . ints)
     fortype "IntSet" (IntSet.fromDistinctAscList . ints)
     fortype "Map Int Int" (Map.fromDistinctAscList . intpairs)
     fortype "IntMap Int" (IntMap.fromDistinctAscList . intpairs)
  where fortype label make =
          scale (\(n,nstr) ->
                   do let title :: String -> String
                          title for = printf "%12s %-20s %s" nstr (label :: String) for
                          encodeDecode en de =
                            (return . (`asTypeOf` make n) . de . force . en . make) n
                      action (title "Allocate")
                             (return (make n))
                      action (title "Encode: Store")
                             (return (Store.encode (force (make n))))
                      action (title "Encode: Cereal")
                             (return (Cereal.encode (force (make n))))
                      action (title "Encode/Decode: Store")
                             (encodeDecode Store.encode Store.decodeEx)
                      action (title "Encode/Decode: Cereal")
                             (encodeDecode Cereal.encode (fromRight . Cereal.decode)))
        scale f =
          mapM_ f
                (map (\x -> (x,commas x))
                     [1000000,2000000,10000000])
        ints n = [1..n] :: [Int]
        intpairs = map (\x -> (x, x)) . ints
        fromRight = either (error "Left") id
