{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- We can build vectors in GLSL using the constructor functions like @vec2@.
-- However, they are /heavily/ overloaded: in effect, you can pass any
-- arguments you like as long as the number of components adds up to the right
-- number. For example, all the following are valid and equivalent:
--
--     vec3(1, 2, 3);
--     vec3(vec2(1, 2), 3);
--     vec3(1, vec2(2, 3));
--     vec3(vec3(1, 2, 3));
--
-- To replicate this, we use a little bit of type-level magic to make sure that
-- I build, say, my @vec4@ 'With' four @GLfloat@ values.
module Shader.Expression.Vector where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Natural, type (+))
import Graphics.Rendering.OpenGL (GLfloat)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V2, V3, V4)
import Shader.Expression.Core (Expr (Expr, toGLSL))

-- | Vectors count towards the required "total" for vector constructors.
-- Everything else counts as one element. This means we can mix scalar floats
-- and vectors of floats to create larger vectors of floats, for example.
--
-- Sometimes, we'll hit an ambiguity: if, for example, a user expresses a
-- vector like @vec4(1, 1, 1, 1)@, we're relying on the appropriate instance
-- for 'Shader.Expression.Constants.Lift' to be picked for each @1@. To make
-- this work, we use an @INCOHERENT@ instance: if the given value can't be
-- determined to be a vector, we assume it is a scalar.
type Size :: Type -> Natural -> Constraint
class Size x n | x -> n

instance (n ~ 2) => Size (V2 x) n

instance (n ~ 3) => Size (V3 x) n

instance (n ~ 4) => Size (V4 x) n

instance {-# INCOHERENT #-} (n ~ 1) => Size x n

-- | Because vectors must be heterogeneous, we have to keep track of the type
-- the target vector will store. This must be consistent across all scalar and
-- vector components in the arguments to the constructor function.
--
-- As with 'Size', the @INCOHERENT@ instance here makes type inference work in
-- the case that one of our arguments has an ambiguous type.
type Inner :: Type -> Type -> Constraint
class Inner xs e | xs -> e

instance (x ~ y) => Inner (V2 x) y

instance (x ~ y) => Inner (V3 x) y

instance (x ~ y) => Inner (V4 x) y

instance {-# INCOHERENT #-} (x ~ y) => Inner x y

-- | A class of tuples with the given number of components of the given type.
-- For example, @( 'V2' 'GLint', 'GLint' )@ is a type @'With' 2 'GLint'@.
type With :: Natural -> Type -> Type -> Constraint
class With n x xs | xs -> n x where
  -- | Convert this type to a list of untyped GLSL expressions to be used as
  -- 'Syntax.Parameters'.
  flatten :: xs -> [Syntax.Expr]

instance (Size x n, Inner x e) => With n e (Expr x) where
  flatten = pure . toGLSL

instance
  (Size x s, Size y t, s + t ~ n, Inner x e, Inner y e) =>
  With n e (Expr x, Expr y)
  where
  flatten (x, y) = [toGLSL x, toGLSL y]

instance
  (Size x s, Size y t, Size z u, s + t + u ~ n, Inner x e, Inner y e, Inner z e) =>
  With n e (Expr x, Expr y, Expr z)
  where
  flatten (x, y, z) = [toGLSL x, toGLSL y, toGLSL z]

instance
  (Size x s, Size y t, Size z u, Size w v, s + t + u + v ~ n, Inner x e, Inner y e, Inner z e, Inner w e) =>
  With n e (Expr x, Expr y, Expr z, Expr w)
  where
  flatten (x, y, z, w) = [toGLSL x, toGLSL y, toGLSL z, toGLSL w]

-- | Construct a @'V2' 'GLfloat'@ from 'GLfloat' components. The following
-- examples should yield the same result:
--
-- >>> vec2 (vec2 (x, y))
-- >>> vec2 (x, y)
vec2 :: (With 2 GLfloat x) => x -> Expr (V2 GLfloat)
vec2 = Expr . Syntax.FunctionCall (Syntax.FuncId "vec2") . Syntax.Params . flatten

-- | Construct a @'V3' 'GLfloat'@ from 'GLfloat' components. All the following
-- examples should yield the same result:
--
-- >>> vec3 (vec3 (x, y, z))
-- >>> vec3 (x, vec2 (y, z))
-- >>> vec3 (vec2 (x, y), z)
-- >>> vec3 (x, y, z)
vec3 :: (With 3 GLfloat x) => x -> Expr (V3 GLfloat)
vec3 = Expr . Syntax.FunctionCall (Syntax.FuncId "vec3") . Syntax.Params . flatten

-- | Construct a @'V4' 'GLfloat'@ from 'GLfloat' components. All the following
-- examples should yield the same result:
--
-- >>> vec4 (vec4 (x, y, z, w))
-- >>> vec4 (x, vec3 (y, z, w))
-- >>> vec4 (vec3 (x, y, z), w)
-- >>> vec4 (x, y, vec2 (z, w))
-- >>> vec4 (x, vec2 (y, z), w)
-- >>> vec4 (vec2 (x, y), z, w)
-- >>> vec4 (x, y, z, w)
vec4 :: (With 4 GLfloat x) => x -> Expr (V4 GLfloat)
vec4 = Expr . Syntax.FunctionCall (Syntax.FuncId "vec4") . Syntax.Params . flatten
