{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression
  ( Expr (toGLSL),

    -- * GLSL Embedding
    fromInteger,
    fromRational,
    lift,

    -- * Vector Constructors
    vec2,
    vec3,
    vec4,
  )
where

import Shader.Expression.Constants (fromInteger, fromRational, lift)
import Shader.Expression.Core (Expr (toGLSL))
import Shader.Expression.Vector (vec2, vec3, vec4)
