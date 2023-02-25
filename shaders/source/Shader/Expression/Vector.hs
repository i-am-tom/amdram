{-# LANGUAGE BlockArguments #-}

-- |
-- Functions for constructing GLSL vectors.
module Shader.Expression.Vector where

import Graphics.Rendering.OpenGL (GLboolean, GLfloat, GLint)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V2, V3, V4)
import Shader.Expression.Core (Expr (Expr, toGLSL))

-- | Construct a @'V2' 'GLboolean'@ from 'GLboolean' components.
bvec2 :: Expr GLboolean -> Expr GLboolean -> Expr (V2 GLboolean)
bvec2 x y = Expr $ Syntax.FunctionCall (Syntax.FuncId "bvec2") do
  Syntax.Params [toGLSL x, toGLSL y]

-- | Construct a @'V3' 'GLboolean'@ from 'GLboolean' components.
bvec3 :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr (V3 GLboolean)
bvec3 x y z = Expr $ Syntax.FunctionCall (Syntax.FuncId "bvec3") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z]

-- | Construct a @'V4' 'GLboolean'@ from 'GLboolean' components.
bvec4 :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr (V4 GLboolean)
bvec4 x y z w = Expr $ Syntax.FunctionCall (Syntax.FuncId "bvec4") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z, toGLSL w]

-- | Construct a @'V2' 'GLint'@ from 'GLint' components.
ivec2 :: Expr GLint -> Expr GLint -> Expr (V2 GLint)
ivec2 x y = Expr $ Syntax.FunctionCall (Syntax.FuncId "ivec2") do
  Syntax.Params [toGLSL x, toGLSL y]

-- | Construct a @'V3' 'GLint'@ from 'GLint' components.
ivec3 :: Expr GLint -> Expr GLint -> Expr GLint -> Expr (V3 GLint)
ivec3 x y z = Expr $ Syntax.FunctionCall (Syntax.FuncId "ivec3") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z]

-- | Construct a @'V4' 'GLint'@ from 'GLint' components.
ivec4 :: Expr GLint -> Expr GLint -> Expr GLint -> Expr GLint -> Expr (V4 GLint)
ivec4 x y z w = Expr $ Syntax.FunctionCall (Syntax.FuncId "ivec4") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z, toGLSL w]

-- | Construct a @'V2' 'GLfloat'@ from 'GLfloat' components.
vec2 :: Expr GLfloat -> Expr GLfloat -> Expr (V2 GLfloat)
vec2 x y = Expr $ Syntax.FunctionCall (Syntax.FuncId "vec2") do
  Syntax.Params [toGLSL x, toGLSL y]

-- | Construct a @'V3' 'GLfloat'@ from 'GLfloat' components.
vec3 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V3 GLfloat)
vec3 x y z = Expr $ Syntax.FunctionCall (Syntax.FuncId "vec3") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z]

-- | Construct a @'V4' 'GLfloat'@ from 'GLfloat' components.
vec4 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V4 GLfloat)
vec4 x y z w = Expr $ Syntax.FunctionCall (Syntax.FuncId "vec4") do
  Syntax.Params [toGLSL x, toGLSL y, toGLSL z, toGLSL w]
