-- |
-- A couple useful helpers for converting in and out of 'Cofree'.
module Control.Comonad.Cofree.Extra where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Fix (Fix (Fix))

-- | Annotate every node of a 'Fix' with a value, thus forming a 'Cofree'.
decorate :: (Functor f) => (f (Fix f) -> x) -> Fix f -> Cofree f x
decorate f (Fix x) = f x :< fmap (decorate f) x

-- | Remove the annotation "head" at each subtree within the 'Cofree'.
decapitate :: (Functor f) => Cofree f x -> Fix f
decapitate (_ :< xs) = Fix (fmap decapitate xs)
