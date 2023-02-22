-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression
  ( Expr (toGLSL),
    lift,
    vec4,
  )
where

import Shader.Expression.Constants (lift)
import Shader.Expression.Core (Expr (toGLSL), vec4)
