{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Data.Refcount
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

main = $(defaultMainGenerator)

infix 4 ==.

(==.) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(==.) = liftA2 (==)

prop_insert_remove :: a ~ Integer => a -> Refcount a -> Bool
prop_insert_remove a = (deleteRef a . insertRef a) ==. Just

prop_insert_remove_refcount :: a ~ Integer => a -> Refcount a -> Bool
prop_insert_remove_refcount a r
    = let i = insertRef a r
          d = deleteRef a i
      in refcount a i == refcount a r + 1
      && (refcount a <$> d) == Just (refcount a i - 1)
