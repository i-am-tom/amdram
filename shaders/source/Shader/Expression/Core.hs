{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression.Core where

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

-- | Create a @vec4@ from four 'GLfloat' 'Expr' components. This will probably
-- be replaced with a class in the near future to reflect the different ways in
-- which GLSL overloads this function.
vec4 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V4 GLfloat)
vec4 (toGLSL -> x) (toGLSL -> y) (toGLSL -> z) (toGLSL -> w) = Expr do
  Syntax.FunctionCall (Syntax.FuncId "vec4") $ Syntax.Params [x, y, z, w]
