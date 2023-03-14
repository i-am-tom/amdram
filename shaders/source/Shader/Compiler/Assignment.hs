{-# LANGUAGE BlockArguments #-}
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
import Data.Some (Some (Some))
import Shader.Expression (Expr)
import Toolbox.Generic (Traverse, ifoldMap)

-- | Structures that contain a set of GL assignments.
type Assignment :: Type -> Constraint
class Assignment x where
  -- | Extract a set of assignments from the given type. The assignments should
  -- be assumed to be unordered - no assignment should rely on the result of
  -- another.
  assignments :: x -> Map String (Some Expr)
  default assignments :: (Traverse IsExpression x) => x -> Map String (Some Expr)
  assignments = ifoldMap @IsExpression \name ->
    foldMap (Map.singleton name) . expression

-- | The default implementation of 'Assignment' relies on the given value being
-- of a type whose constructors are all record constructors. These fields
-- should all be some sort of 'Expr' /or/ a 'Maybe'-wrapped 'Expr'.
type IsExpression :: Type -> Constraint
class IsExpression x where
  -- | Transform a record field and its value into a map of expressions.
  expression :: x -> Maybe (Some Expr)

instance IsExpression (Expr x) where
  expression = Just . Some

instance (IsExpression x) => IsExpression (Maybe x) where
  expression = (>>= expression)
