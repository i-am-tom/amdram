{-# LANGUAGE BlockArguments #-}

-- |
-- A couple useful helpers for converting in and out of 'Cofree'.
module Control.Comonad.Cofree.Extra
  ( note,
    hush,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Fix (Fix (Fix, unFix))
import Data.Functor.Foldable (hylo)
import Prelude (Functor, flip)

-- | Annotate every node of a 'Fix' with a value, thus forming a 'Cofree'.
note :: (Functor f) => (f (Cofree f x) -> x) -> Fix f -> Cofree f x
note f = flip hylo unFix \x -> f x :< x

-- | Remove the annotation "head" at each subtree within the 'Cofree'.
hush :: (Functor f) => Cofree f x -> Fix f
hush = hylo Fix \(_ :< x) -> x
