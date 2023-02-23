{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Typed addition for GLSL.
module Shader.Expression.Addition where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLfloat)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V4)
import Shader.Expression.Core (Expr (Expr, toGLSL))

-- | Add together two GLSL expressions.
(+) :: (Add x y z) => Expr x -> Expr y -> Expr z
(+) (toGLSL -> x) (toGLSL -> y) = Expr (Syntax.Add x y)

-- | In GLSL, we can add scalars to vectors and matrices to perform
-- component-wise changes. Because of this, the output type is computed as a
-- function of the input types.
type Add :: Type -> Type -> Type -> Constraint
class Add left right output | left right -> output

instance Add GLfloat GLfloat GLfloat

instance Add (V4 GLfloat) GLfloat (V4 GLfloat)

instance Add GLfloat (V4 GLfloat) (V4 GLfloat)

instance Add (V4 GLfloat) (V4 GLfloat) (V4 GLfloat)
