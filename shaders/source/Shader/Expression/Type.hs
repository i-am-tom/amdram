{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

-- | Types of values within GLSL.
module Shader.Expression.Type where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLboolean, GLdouble, GLfloat, GLint, GLuint)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V2, V3, V4)

-- | In order to compute intermediate results in GLSL, we need to define types
-- for them. This class defines a syntactical representation of a value's type
-- within GLSL.
type Typed :: Type -> Constraint
class Typed x where
  -- | What is the GLSL type declaration for this type? Meant to be used with
  -- type applications, i.e. @typeOf @GLdouble@.
  typeOf :: Syntax.TypeSpecifier

instance Typed GLboolean where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Float Nothing

instance Typed GLdouble where
  typeOf = Syntax.TypeSpec (Just Syntax.HighP) do
    Syntax.TypeSpecNoPrecision Syntax.Float Nothing

instance Typed GLfloat where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Float Nothing

instance Typed GLint where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Float Nothing

instance Typed GLuint where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.UInt Nothing

instance Typed (V2 GLboolean) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Vec2 Nothing

instance Typed (V3 GLboolean) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Vec3 Nothing

instance Typed (V4 GLboolean) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Vec4 Nothing

instance Typed (V2 GLfloat) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Vec2 Nothing

instance Typed (V3 GLfloat) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Vec3 Nothing

instance Typed (V4 GLfloat) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.Vec4 Nothing

instance Typed (V2 GLint) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.IVec2 Nothing

instance Typed (V3 GLint) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.IVec3 Nothing

instance Typed (V4 GLint) where
  typeOf = Syntax.TypeSpec Nothing do
    Syntax.TypeSpecNoPrecision Syntax.IVec4 Nothing
