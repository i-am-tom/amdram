{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Boolean logic for GLSL.
module Shader.Expression.Logic where

import Graphics.Rendering.OpenGL (GLboolean)
import Shader.Expression.Core (Expr (toAST), Expression (And, BoolConstant, Or, Selection), expr_)
import Shader.Expression.Type (Typed)

-- | Boolean 'True'.
true :: Expr GLboolean
true = expr_ (BoolConstant True)

-- | Boolean 'False'.
false :: Expr GLboolean
false = expr_ (BoolConstant False)

-- | Boolean conjunction (@AND@).
(&&) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(&&) (toAST -> x) (toAST -> y) = expr_ (And x y)

-- | Boolean disjunction (@OR@).
(||) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(||) (toAST -> x) (toAST -> y) = expr_ (Or x y)

-- | AST "selection". When @RebindableSyntax@ is enabled, regular
-- @if@/@then@/@else@ syntax can compile to this function.
ifThenElse :: (Typed x) => Expr GLboolean -> Expr x -> Expr x -> Expr x
ifThenElse (toAST -> p) (toAST -> x) (toAST -> y) = expr_ (Selection p x y)
