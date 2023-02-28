-- |
-- A function for an implicit GLSL conversion.
module Shader.Expression.Cast where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Linear (V2, V3, V4)
import Shader.Expression.Core (Expr (unExpr), ExprF (Cast), unsafeLift)
import Shader.Expression.Type (Typed)

-- | GLSL supports implicit conversions: if a @uint@ is expected and you give
-- an @int@, then the @int@ can be implicitly converted into a @uint@. Because
-- we /want/ types, we have to make our implicit conversions... well, explicit.
-- This should be zero cost: in theory, @cast x@ should have the same GLSL
-- representation as @x@.
cast :: (Cast x y, Typed y) => Expr x -> Expr y
cast = unsafeLift . Cast . unExpr

-- | Types that can be implicitly converted into other types.
type Cast :: Type -> Type -> Constraint
class Cast x y

instance Cast GLint GLfloat

instance Cast (V2 GLint) (V2 GLfloat)

instance Cast (V3 GLint) (V3 GLfloat)

instance Cast (V4 GLint) (V4 GLfloat)
