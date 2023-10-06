-- | This module provides utilities for computing hashes based on the
-- structural definitions of datatypes. The purpose of this is to
-- provide a mechanism for tagging serialized data in such a way that
-- deserialization issues can be anticipated.
module Data.Store.TypeHash
    ( Tagged(..)
    , TypeHash
    , HasTypeHash(..)
    -- * TH for generating HasTypeHash instances
    , mkHasTypeHash
    , mkManyHasTypeHash
    ) where

import Data.Store.TypeHash.Internal
