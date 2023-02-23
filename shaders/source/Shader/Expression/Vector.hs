{-# LANGUAGE BlockArguments #-}

-- |
-- Functions for constructing GLSL vectors.
module Shader.Expression.Vector where

import Graphics.Rendering.OpenGL (GLfloat)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V4)
import Shader.Expression.Core (Expr (Expr, toGLSL))

-- | Construct a @'V4' 'GLfloat'@ from 'GLfloat' components.
vec4 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V4 GLfloat)
vec4 x y z w = Expr $ Syntax.FunctionCall (Syntax.FuncId "vec4") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z, toGLSL w]