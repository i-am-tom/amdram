-- |
-- A function for an implicit GLSL conversion.
module Shader.Expression.Cast where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Linear (V4)
import Shader.Expression.Core (Expr (Cast))
import Shader.Expression.Type (Typed)

-- | GLSL supports implicit conversions: if a @uint@ is expected and you give
-- an @int@, then the @int@ can be implicitly converted into a @uint@. Because
-- we /want/ types, we have to make our implicit conversions... well, explicit.
-- This should be zero cost: in theory, @cast x@ should have the same GLSL
-- representation as @x@.
cast :: (Cast x y, Typed y) => Expr x -> Expr y
cast = Cast

-- | Types that can be implicitly converted into other types.
type Cast :: Type -> Type -> Constraint
class Cast x y

instance Cast GLint GLfloat

instance (Cast x y) => Cast (V4 x) (V4 y)
