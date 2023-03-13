{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Helpful generic utilities.
module Toolbox.Generic where

import Control.Applicative (liftA2)
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Any standard ADT can be "traversed" with a function we apply to every
-- field on the current constructor, supplying the field name along with it.
type Traverse :: (Type -> Constraint) -> Type -> Constraint
class Traverse c s where
  -- | Apply a function to every field of every constructor, giving them their
  -- own field names. Note that this assumes constructors are all records.
  itraverse :: (Applicative f) => (forall a. (c a) => String -> a -> f a) -> s -> f s

-- | The generic machinery behind 'Traverse'.
type GTraverse :: (Type -> Constraint) -> (Type -> Type) -> Constraint
class GTraverse c rep where
  -- | An implementation of 'itraverse' over generic representations.
  gitraverse :: (Applicative f) => (forall a. (c a) => String -> a -> f a) -> rep x -> f (rep x)

instance (GTraverse c x) => GTraverse c (D1 meta x) where
  gitraverse f = fmap M1 . gitraverse @c f . unM1

instance (GTraverse c x) => GTraverse c (C1 meta x) where
  gitraverse f = fmap M1 . gitraverse @c f . unM1

instance (GTraverse c l, GTraverse c r) => GTraverse c (l :*: r) where
  gitraverse f (l :*: r) = liftA2 (:*:) (gitraverse @c f l) (gitraverse @c f r)

instance (GTraverse c l, GTraverse c r) => GTraverse c (l :+: r) where
  gitraverse f = \case
    L1 l -> fmap L1 (gitraverse @c f l)
    R1 r -> fmap R1 (gitraverse @c f r)

instance
  (KnownSymbol name, c x, meta ~ 'MetaSel ('Just name) i d k) =>
  GTraverse c (S1 meta (K1 r x))
  where
  gitraverse f = fmap (M1 . K1) . f key . unK1 . unM1
    where
      key = symbolVal (Proxy @name)

instance (Generic x, GTraverse c (Rep x)) => Traverse c x where
  itraverse f = fmap to . gitraverse @c f . from

-- | A version of 'itraverse' that ignores the record field name.
traverse :: forall c f s. (Applicative f, Traverse c s) => (forall a. (c a) => a -> f a) -> s -> f s
traverse f = itraverse @c (const f)

-- | Fold over a record type with access to the field names.
ifoldMap :: forall c s m. (Traverse c s, Monoid m) => (forall a. (c a) => String -> a -> m) -> s -> m
ifoldMap f = getConst . itraverse @c (\s -> Const . f s)

-- | Fold over a record type.
foldMap :: forall c s m. (Traverse c s, Monoid m) => (forall a. (c a) => a -> m) -> s -> m
foldMap f = ifoldMap @c (const f)

-- | A version of 'itraverse' that doesn't produce an 'Applicative' effect.
imap :: forall c s. (Traverse c s) => (forall a. (c a) => String -> a -> a) -> s -> s
imap f = runIdentity . itraverse @c (\s -> Identity . f s)

-- | A version of 'imap' that ignores the record field name.
map :: forall c s. (Traverse c s) => (forall a. (c a) => a -> a) -> s -> s
map f = imap @c (const f)
