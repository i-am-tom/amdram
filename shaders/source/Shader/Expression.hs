{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression
  ( Expr (toGLSL),

    -- * GLSL Embedding
    fromInteger,
    fromRational,
    lift,

    -- * Implicit conversions
    Cast,
    cast,

    -- * Vector Constructors
    ivec2,
    ivec3,
    ivec4,
    vec2,
    vec3,
    vec4,

    -- * Arithmetic
    Add,
    (+),
  )
where

import Shader.Expression.Addition (Add, (+))
import Shader.Expression.Cast (Cast, cast)
import Shader.Expression.Constants (fromInteger, fromRational, lift)
import Shader.Expression.Core (Expr (toGLSL))
import Shader.Expression.Vector (ivec2, ivec3, ivec4, vec2, vec3, vec4)
