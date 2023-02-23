{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression.Core where

import Data.Kind (Type)
import GHC.Records (HasField (getField))
import Language.GLSL.Syntax qualified as Syntax
import Linear (R1, R2, R3, R4)

-- | An expression is a GLSL value that has a type. In practice, because all
-- computation will be performed on the GPU once we've compiled the shader, the
-- type here is entirely phantom - it's just there to make the operations
-- well typed.
type Expr :: Type -> Type
newtype Expr x = Expr {toGLSL :: Syntax.Expr}

instance (x ~ v e, R1 v) => HasField "x" (Expr x) (Expr e) where
  getField (toGLSL -> xs) = Expr (Syntax.FieldSelection xs "x")

instance (x ~ v e, R2 v) => HasField "y" (Expr x) (Expr e) where
  getField (toGLSL -> xs) = Expr (Syntax.FieldSelection xs "y")

instance (x ~ v e, R3 v) => HasField "z" (Expr x) (Expr e) where
  getField (toGLSL -> xs) = Expr (Syntax.FieldSelection xs "z")

instance (x ~ v e, R4 v) => HasField "w" (Expr x) (Expr e) where
  getField (toGLSL -> xs) = Expr (Syntax.FieldSelection xs "w")
