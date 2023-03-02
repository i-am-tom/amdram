{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression
  ( Expr,
    toGLSL,

    -- * GLSL Embedding
    fromInteger,
    fromRational,
    lift,

    -- * Implicit conversions
    Cast,
    cast,

    -- * Vector Constructors
    bvec2,
    bvec3,
    bvec4,
    ivec2,
    ivec3,
    ivec4,
    vec2,
    vec3,
    vec4,

    -- * Arithmetic
    Add,
    (+),

    -- * Boolean logic
    true,
    false,
    (&&),
    (||),
    ifThenElse,

    -- * Optimisation
    share,
  )
where

import Shader.Expression.Addition (Add, (+))
import Shader.Expression.Cast (Cast, cast)
import Shader.Expression.Constants (fromInteger, fromRational, lift)
import Shader.Expression.Core (Expr, toGLSL)
import Shader.Expression.Logic (false, ifThenElse, true, (&&), (||))
import Shader.Expression.Share (share)
import Shader.Expression.Vector (bvec2, bvec3, bvec4, ivec2, ivec3, ivec4, vec2, vec3, vec4)
