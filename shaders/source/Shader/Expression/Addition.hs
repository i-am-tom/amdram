{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Typed addition for GLSL.
module Shader.Expression.Addition where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLfloat)
import Linear (V4)
import Shader.Expression.Core (Expr (Add))
import Shader.Expression.Type (Typed)

-- | Add together two GLSL expressions.
(+) :: (Add x y z, Typed z) => Expr x -> Expr y -> Expr z
(+) = Add

-- | In GLSL, we can add scalars to vectors and matrices to perform
-- component-wise changes. Because of this, the output type is computed as a
-- function of the input types.
type Add :: Type -> Type -> Type -> Constraint
class Add left right output | left right -> output

instance Add GLfloat GLfloat GLfloat

instance Add (V4 GLfloat) GLfloat (V4 GLfloat)

instance Add GLfloat (V4 GLfloat) (V4 GLfloat)

instance Add (V4 GLfloat) (V4 GLfloat) (V4 GLfloat)
