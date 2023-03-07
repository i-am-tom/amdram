{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Extra utilities for @barbies@.
module Barbies.Extra where

import Barbies (AllB, ApplicativeB, ConstraintsB, FunctorB, TraversableB)
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
type Aligned :: ((Type -> Type) -> Type) -> Type -> Type
newtype Aligned b x = Aligned {unAligned :: b (Const x)}

instance (FunctorB b) => Functor (Aligned b) where
  fmap f = Aligned . B.bmap (Const . f . getConst) . unAligned

instance (ApplicativeB b) => Applicative (Aligned b) where
  Aligned fs <*> Aligned xs = Aligned (B.bzipWith go fs xs)
    where
      go (Const f) (Const x) = Const (f x)
  pure x = Aligned (B.bpure (Const x))

instance (TraversableB b) => Foldable (Aligned b) where
  foldMap f = B.bfoldMap (f . getConst) . unAligned

instance (TraversableB b) => Traversable (Aligned b) where
  traverse f = fmap Aligned . B.btraverse (fmap Const . f . getConst) . unAligned

-- | Convert a regular barbie to an aligned barbie using an unconstrained
-- function.
align :: (FunctorB b) => (forall a. f a -> x) -> b f -> Aligned b x
align f = Aligned . B.bmap (Const . f)

-- | Convert a regular barbie to an aligned barbie using a constrained
-- function.
alignC :: forall c b f x. (AllB c b, ConstraintsB b, FunctorB b) => (forall a. (c a) => f a -> x) -> b f -> Aligned b x
alignC f = Aligned . B.bmapC @c (Const . f)

-- | Convert an aligned barbie back to a regular barbie using an unconstrained
-- function.
unalign :: (FunctorB b) => (forall a. x -> g a) -> Aligned b x -> b g
unalign f = B.bmap (f . getConst) . unAligned

-- | Convert an aligned barbie back to a regular barbie using a constrained
-- function.
unalignC :: forall c b g x. (AllB c b, ConstraintsB b, FunctorB b) => (forall a. (c a) => x -> g a) -> Aligned b x -> b g
unalignC f = B.bmapC @c (f . getConst) . unAligned
