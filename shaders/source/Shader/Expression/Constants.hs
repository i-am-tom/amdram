-- |
-- Utilities for lifting constants into GLSL.
module Shader.Expression.Constants
  ( Lift (lift),
    fromInteger,
    fromRational,
  )
where

import Data.Kind (Constraint, Type)
import Graphics.Rendering.OpenGL (GLdouble, GLfloat, GLint, GLuint)
import Language.GLSL.Syntax qualified as Syntax
import Linear (V4 (V4))
import Shader.Expression.Core (Expr (Expr))
import Shader.Expression.Vector (vec4)
import Prelude hiding (fromInteger, fromRational)
import Prelude qualified

-- | Lift any expressable type into @GLSL@.
type Lift :: Type -> Constraint
class Lift x where
  -- | Lift a value into @GLSL@.
  lift :: x -> Expr x

instance Lift GLdouble where
  lift = Expr . Syntax.FloatConstant . realToFrac

instance Lift GLfloat where
  lift = Expr . Syntax.FloatConstant

instance Lift GLint where
  lift = Expr . Syntax.IntConstant Syntax.Decimal . fromIntegral

instance Lift GLuint where
  lift = Expr . Syntax.IntConstant Syntax.Decimal . fromIntegral

instance Lift (V4 GLfloat) where
  lift (V4 x y z w) = vec4 (lift x) (lift y) (lift z) (lift w)

-- | @RebindableSyntax@ function for integer literals.
fromInteger :: (Lift x, Num x) => Integer -> Expr x
fromInteger = lift . Prelude.fromInteger

-- | @RebindableSyntax@ function for rational literals.
fromRational :: (Lift x, Fractional x) => Rational -> Expr x
fromRational = lift . Prelude.fromRational
