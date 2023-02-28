{-# LANGUAGE BlockArguments #-}

-- |
-- Functions for constructing GLSL vectors.
module Shader.Expression.Vector where

import Graphics.Rendering.OpenGL (GLboolean, GLfloat, GLint)
import Linear (V2, V3, V4)
import Shader.Expression.Core (Expr (Expr), ExprF (FunctionCall), unsafeLift)

-- | Construct a @'V2' 'GLboolean'@ from 'GLboolean' components.
bvec2 :: Expr GLboolean -> Expr GLboolean -> Expr (V2 GLboolean)
bvec2 (Expr x) (Expr y) = unsafeLift (FunctionCall "bvec2" [x, y])

-- | Construct a @'V3' 'GLboolean'@ from 'GLboolean' components.
bvec3 :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr (V3 GLboolean)
bvec3 (Expr x) (Expr y) (Expr z) = unsafeLift (FunctionCall "bvec3" [x, y, z])

-- | Construct a @'V4' 'GLboolean'@ from 'GLboolean' components.
bvec4 :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr (V4 GLboolean)
bvec4 (Expr x) (Expr y) (Expr z) (Expr w) = unsafeLift (FunctionCall "bvec4" [x, y, z, w])

-- | Construct a @'V2' 'GLint'@ from 'GLint' components.
ivec2 :: Expr GLint -> Expr GLint -> Expr (V2 GLint)
ivec2 (Expr x) (Expr y) = unsafeLift (FunctionCall "ivec2" [x, y])

-- | Construct a @'V3' 'GLint'@ from 'GLint' components.
ivec3 :: Expr GLint -> Expr GLint -> Expr GLint -> Expr (V3 GLint)
ivec3 (Expr x) (Expr y) (Expr z) = unsafeLift (FunctionCall "ivec3" [x, y, z])

-- | Construct a @'V4' 'GLint'@ from 'GLint' components.
ivec4 :: Expr GLint -> Expr GLint -> Expr GLint -> Expr GLint -> Expr (V4 GLint)
ivec4 (Expr x) (Expr y) (Expr z) (Expr w) = unsafeLift (FunctionCall "ivec4" [x, y, z, w])

-- | Construct a @'V2' 'GLfloat'@ from 'GLfloat' components.
vec2 :: Expr GLfloat -> Expr GLfloat -> Expr (V2 GLfloat)
vec2 (Expr x) (Expr y) = unsafeLift (FunctionCall "vec2" [x, y])

-- | Construct a @'V3' 'GLfloat'@ from 'GLfloat' components.
vec3 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V3 GLfloat)
vec3 (Expr x) (Expr y) (Expr z) = unsafeLift (FunctionCall "vec3" [x, y, z])

-- | Construct a @'V4' 'GLfloat'@ from 'GLfloat' components.
vec4 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V4 GLfloat)
vec4 (Expr x) (Expr y) (Expr z) (Expr w) = unsafeLift (FunctionCall "vec4" [x, y, z, w])
