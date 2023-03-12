{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- An interface to structures that represent assignments within a GLSL shader.
module Shader.Compiler.Assignment where

import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Some (Some (Some))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Shader.Expression (Expr)

-- | Barbie structures that can be converted to assignments in a GLSL
-- shader.
type Assignment :: Type -> Constraint
class Assignment x where
  -- | Convert a structure into an assignment map. These assignments should be
  -- assumed to be unordered - no assignment should reference the result of
  -- another - as we will attempt to recover shared values later.
  assignments :: x -> Map String (Some Expr)
  default assignments :: (Generic x, GAssignment (Rep x)) => x -> Map String (Some Expr)
  assignments = gassignments . from

-- | We can derive 'Assignment' generically if we assume that the values will
-- be assigned to variables whose names match the field names in the Barbie
-- type.
type GAssignment :: (Type -> Type) -> Constraint
class GAssignment rep where
  -- | A generic instance for 'assignments'.
  gassignments :: rep x -> Map String (Some Expr)

instance (GAssignment inner) => GAssignment (M1 D meta inner) where
  gassignments = gassignments . unM1

instance (GAssignment inner) => GAssignment (M1 C meta inner) where
  gassignments = gassignments . unM1

instance (GAssignment l, GAssignment r) => GAssignment (l :*: r) where
  gassignments (left :*: right) = gassignments left <> gassignments right

instance
  (KnownSymbol name, meta ~ 'MetaSel ('Just name) i d c) =>
  GAssignment (M1 S meta (K1 r (Expr x)))
  where
  gassignments (M1 (K1 x)) = Map.singleton (symbolVal (Proxy @name)) (Some x)

instance
  (KnownSymbol name, meta ~ 'MetaSel ('Just name) i d c) =>
  GAssignment (M1 S meta (K1 r (Maybe (Expr x))))
  where
  gassignments (M1 (K1 xs)) = case xs of
    Just x -> Map.singleton (symbolVal (Proxy @name)) (Some x)
    Nothing -> mempty
