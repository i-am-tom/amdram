{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Extra utilities for @barbies@.
module Barbies.Extra where

import Barbies (AllB, ApplicativeB (bpure), ConstraintsB, FunctorB (bmap), TraversableB, bfoldMap, bmapC, btraverse, bzipWith)
import Barbies qualified as B
import Data.Functor.Const (Const (Const, getConst))
import Data.Kind (Type)
import GHC.Generics (Generic)

-- | A barbie type that only contains a single value.
type One :: Type -> (Type -> Type) -> Type
newtype One x f = One {runOne :: f x}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ApplicativeB, ConstraintsB, FunctorB, TraversableB)

-- | Let's imagine a very specific use case: I want to use
-- @Data.Reify.reifyGraphs@ to take @MyStructure Expr@ and produce some shared
-- bindings among the different properties. @reifyGraphs@ assumes that the
-- structure is 'Traversable', but we want to use barbie types so that we can
-- have a set of heterogeneous typed expressions.
--
-- 'Aligned' means we can briefly "untype" our 'Expr', perform observable
-- sharing using the 'Traversable' instance below, and then "retype" our 'Expr'
-- while collecting helpful intermediate bindings.
type Aligned :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Type
newtype Aligned b f x = Aligned {unAligned :: b (Const (f x))}

instance (FunctorB b, Functor f) => Functor (Aligned b f) where
  fmap f = Aligned . bmap (Const . fmap f . getConst) . unAligned

instance (ApplicativeB b, Applicative f) => Applicative (Aligned b f) where
  Aligned fs <*> Aligned xs = Aligned (bzipWith go fs xs)
    where
      go (Const f) (Const x) = Const (f <*> x)
  pure x = Aligned (bpure (Const (pure x)))

instance (TraversableB b, Foldable f) => Foldable (Aligned b f) where
  foldMap f = bfoldMap (foldMap f . getConst) . unAligned

instance (TraversableB b, Traversable f) => Traversable (Aligned b f) where
  traverse f = fmap Aligned . btraverse (fmap Const . traverse f . getConst) . unAligned

-- | Convert a regular barbie to an aligned barbie using an unconstrained
-- function.
align :: (FunctorB b) => (forall a. f a -> g x) -> b f -> Aligned b g x
align f = Aligned . bmap (Const . f)

-- | Convert a regular barbie to an aligned barbie using a constrained
-- function.
alignC :: forall c b f g x. (AllB c b, ConstraintsB b, FunctorB b) => (forall a. (c a) => f a -> g x) -> b f -> Aligned b g x
alignC f = Aligned . bmapC @c (Const . f)

-- | Convert an aligned barbie back to a regular barbie using an unconstrained
-- function.
unalign :: (FunctorB b) => (forall a. f x -> g a) -> Aligned b f x -> b g
unalign f = bmap (f . getConst) . unAligned

-- | Convert an aligned barbie back to a regular barbie using a constrained
-- function.
unalignC :: forall c b f g x. (AllB c b, ConstraintsB b, FunctorB b) => (forall a. (c a) => f x -> g a) -> Aligned b f x -> b g
unalignC f = bmapC @c (f . getConst) . unAligned
