{-# LANGUAGE BlockArguments #-}

-- |
-- Functions for constructing GLSL vectors.
module Shader.Expression.Vector where

import Data.Some (Some (Some))
import Graphics.Rendering.OpenGL (GLboolean, GLfloat, GLint)
import Linear (V2, V3, V4)
import Shader.Expression.Core (Expr (FunctionCall))

-- | Construct a @'V2' 'GLboolean'@ from 'GLboolean' components.
bvec2 :: Expr GLboolean -> Expr GLboolean -> Expr (V2 GLboolean)
bvec2 x y = FunctionCall "bvec2" [Some x, Some y]

-- | Construct a @'V3' 'GLboolean'@ from 'GLboolean' components.
bvec3 :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr (V3 GLboolean)
bvec3 x y z = FunctionCall "bvec3" [Some x, Some y, Some z]

-- | Construct a @'V4' 'GLboolean'@ from 'GLboolean' components.
bvec4 :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr GLboolean -> Expr (V4 GLboolean)
bvec4 x y z w = FunctionCall "bvec4" [Some x, Some y, Some z, Some w]

-- | Construct a @'V2' 'GLint'@ from 'GLint' components.
ivec2 :: Expr GLint -> Expr GLint -> Expr (V2 GLint)
ivec2 x y = FunctionCall "ivec2" [Some x, Some y]

-- | Construct a @'V3' 'GLint'@ from 'GLint' components.
ivec3 :: Expr GLint -> Expr GLint -> Expr GLint -> Expr (V3 GLint)
ivec3 x y z = FunctionCall "ivec3" [Some x, Some y, Some z]

-- | Construct a @'V4' 'GLint'@ from 'GLint' components.
ivec4 :: Expr GLint -> Expr GLint -> Expr GLint -> Expr GLint -> Expr (V4 GLint)
ivec4 x y z w = FunctionCall "ivec4" [Some x, Some y, Some z, Some w]

-- | Construct a @'V2' 'GLfloat'@ from 'GLfloat' components.
vec2 :: Expr GLfloat -> Expr GLfloat -> Expr (V2 GLfloat)
vec2 x y = FunctionCall "vec2" [Some x, Some y]

-- | Construct a @'V3' 'GLfloat'@ from 'GLfloat' components.
vec3 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V3 GLfloat)
vec3 x y z = FunctionCall "vec3" [Some x, Some y, Some z]

-- | Construct a @'V4' 'GLfloat'@ from 'GLfloat' components.
vec4 :: Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr GLfloat -> Expr (V4 GLfloat)
vec4 x y z w = FunctionCall "vec4" [Some x, Some y, Some z, Some w]
