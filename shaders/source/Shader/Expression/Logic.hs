{-# LANGUAGE BlockArguments #-}

-- |
-- Boolean logic for GLSL.
module Shader.Expression.Logic where

import Graphics.Rendering.OpenGL (GLboolean)
import Shader.Expression.Core (Expr (And, BoolConstant, Or, Selection))
import Shader.Expression.Type (Typed)

-- | Boolean 'True'.
true :: Expr GLboolean
true = BoolConstant True

-- | Boolean 'False'.
false :: Expr GLboolean
false = BoolConstant False

-- | Boolean conjunction (@AND@).
(&&) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(&&) = And

-- | Boolean disjunction (@OR@).
(||) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(||) = Or

-- | AST "selection". When @RebindableSyntax@ is enabled, regular
-- @if@/@then@/@else@ syntax can compile to this function.
ifThenElse :: (Typed x) => Expr GLboolean -> Expr x -> Expr x -> Expr x
ifThenElse = Selection
