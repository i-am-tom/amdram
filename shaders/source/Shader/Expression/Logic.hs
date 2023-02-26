{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Boolean logic for GLSL.
module Shader.Expression.Logic where

import Data.Fix (Fix (Fix))
import Graphics.Rendering.OpenGL (GLboolean)
import Shader.Expression.Core (Expr (Expr, toAST), Expression (And, BoolConstant, Or, Selection))

-- | Boolean 'True'.
true :: Expr GLboolean
true = Expr (Fix (BoolConstant True))

-- | Boolean 'False'.
false :: Expr GLboolean
false = Expr (Fix (BoolConstant False))

-- | Boolean conjunction (@AND@).
(&&) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(&&) (toAST -> x) (toAST -> y) = Expr (Fix (And x y))

-- | Boolean disjunction (@OR@).
(||) :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
(||) (toAST -> x) (toAST -> y) = Expr (Fix (Or x y))

-- | AST "selection". When @RebindableSyntax@ is enabled, regular
-- @if@/@then@/@else@ syntax can compile to this function.
ifThenElse :: Expr GLboolean -> Expr x -> Expr x -> Expr x
ifThenElse (toAST -> p) (toAST -> x) (toAST -> y) = Expr (Fix (Selection p x y))
