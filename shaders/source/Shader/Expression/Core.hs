{-# LANGUAGE BlockArguments #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression.Core
  ( Expr (Expr, toGLSL),
  )
where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLfloat)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V4)

-- | An expression is a GLSL value that has a type. In practice, because all
-- computation will be performed on the GPU once we've compiled the shader, the
-- type here is entirely phantom - it's just there to make the operations
-- well typed.
type Expr :: Type -> Type
newtype Expr x = Expr {toGLSL :: Syntax.Expr}
