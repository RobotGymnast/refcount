module Data.Refcount ( Refcount (..)
                     , refcounted
                     , insertRef
                     , deleteRef
                     , removeRef
                     ) where

import Control.Applicative ((<$>))
import Data.Monoid
import Data.Hashable
import Data.HashMap.Strict as HashMap

-- | Maintain a collection of objects with duplication counts.
newtype Refcount a = Refcount { unRefcount :: HashMap a Int }

instance (Hashable a, Eq a) => Monoid (Refcount a) where
  mempty = Refcount empty
  mappend (Refcount a) (Refcount b) = Refcount $ unionWith (+) a b

-- | Retrieve the refcounted objects.
refcounted :: Refcount a -> [a]
refcounted = keys . unRefcount

-- | Insert an object and increment its count.
insertRef :: (Hashable a, Eq a) => a -> Refcount a -> Refcount a
insertRef a (Refcount m) = Refcount $ insertWith (+) a 1 m

-- | Remove a reference to an element and decrease its count, possibly removing it from the set.
-- Returns @Nothing@ if the element wasn't found, or @Just set@.
deleteRef :: (Hashable a, Eq a) => a -> Refcount a -> Maybe (Refcount a)
deleteRef a r = snd <$> removeRef a r

-- | Remove a reference to an element and decrease its count, possibly removing it from the set.
-- Returns @Nothing@ if the element wasn't found, or @Just (wasRemoved, set)@.
removeRef :: (Hashable a, Eq a) => a -> Refcount a -> Maybe (Bool, Refcount a)
removeRef a (Refcount m) = fmap decRef $ HashMap.lookup a m
  where decRef count = if count > 1
                       then (False, Refcount $ adjust (subtract 1) a m)
                       else (True, Refcount $ delete a m)
