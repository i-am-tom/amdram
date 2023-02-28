{-# LANGUAGE BlockArguments #-}

-- |
-- Boolean logic for GLSL.
module Shader.Expression.Logic where

import Graphics.Rendering.OpenGL (GLboolean)
import Shader.Expression.Core (Expr (Expr), ExprF (And, BoolConstant, Or, Selection), unsafeLift)
import Shader.Expression.Type (Typed)

-- | Boolean 'True'.
true :: Expr GLboolean
true = unsafeLift (BoolConstant True)

-- | Boolean 'False'.
false :: Expr GLboolean
false = unsafeLift (BoolConstant False)

-- | Boolean conjunction (@AND@).
(&&) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(&&) (Expr x) (Expr y) = unsafeLift (And x y)

-- | Boolean disjunction (@OR@).
(||) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(||) (Expr x) (Expr y) = unsafeLift (Or x y)

-- | AST "selection". When @RebindableSyntax@ is enabled, regular
-- @if@/@then@/@else@ syntax can compile to this function.
ifThenElse :: (Typed x) => Expr GLboolean -> Expr x -> Expr x -> Expr x
ifThenElse (Expr p) (Expr x) (Expr y) = unsafeLift (Selection p x y)
