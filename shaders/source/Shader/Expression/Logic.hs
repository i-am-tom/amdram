{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Boolean logic for GLSL.
module Shader.Expression.Logic where

import Graphics.Rendering.OpenGL (GLboolean)
import Language.GLSL.Syntax qualified as Syntax
import Shader.Expression.Core (Expr (Expr), toGLSL)

-- | Boolean 'True'.
true :: Expr GLboolean
true = Expr (Syntax.BoolConstant True)

-- | Boolean 'False'.
false :: Expr GLboolean
false = Expr (Syntax.BoolConstant False)

-- | Boolean conjunction (@AND@).
(&&) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(&&) (toGLSL -> x) (toGLSL -> y) = Expr (Syntax.And x y)

-- | Boolean disjunction (@OR@).
(||) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(||) (toGLSL -> x) (toGLSL -> y) = Expr (Syntax.Or x y)

-- | GLSL "selection". When @RebindableSyntax@ is enabled, regular
-- @if@/@then@/@else@ syntax can compile to this function.
ifThenElse :: Expr GLboolean -> Expr x -> Expr x -> Expr x
ifThenElse (toGLSL -> p) (toGLSL -> x) (toGLSL -> y) =
  Expr (Syntax.Selection p x y)
