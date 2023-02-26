{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression.Core
  ( Expr (..),
    toGLSL,
  )
where

import Data.Kind (Type)
import Data.Some (Some)
import Data.Some qualified as Some
import GHC.Records (HasField (getField))
import Graphics.Rendering.OpenGL (GLboolean, GLfloat, GLint)
import Language.GLSL.Syntax qualified as Syntax
import Linear (R1, R2, R3, R4)
import Shader.Expression.Type (Typed)

-- | An expression is a GLSL value that has a type. We keep track of the type
-- of the expression and all its subexpressions in case we want to factor them
-- out as intermediate bindings.
type Expr :: Type -> Type
data Expr x where
  -- | An integer constant. We can 'Cast' this to @uint@, @float@, or @double@,
  -- which means we can use 'Shader.Expression.fromInteger' to produce any of
  -- these types.
  IntConstant :: Integer -> Expr GLint
  -- | A floating point constant. We can 'Cast' this to a @double@, so either
  -- type can be produced using 'Shader.Expression.fromRational'.
  FloatConstant :: Float -> Expr GLfloat
  -- | A boolean constant. 'GLboolean' exists in memory as a 'Data.Word.Word8',
  -- whose @1@ and @0@ values represent 'True' and 'False' respectively.
  BoolConstant :: Bool -> Expr GLboolean
  -- | An implicit conversion from one type to another according to the GLSL
  -- implicit conversion rules.
  Cast :: (Typed y) => Expr x -> Expr y
  -- | Use a swizzle mask to select one or more fields from a GLSL vector.
  FieldSelection :: (Typed y) => String -> Expr x -> Expr y
  -- | Call a GLSL function with the given arguments.
  FunctionCall :: (Typed t) => String -> [Some Expr] -> Expr t
  -- | Add together two GLSL scalars or vectors.
  Add :: (Typed z) => Expr x -> Expr y -> Expr z
  -- | Compute the logical conjunction of two GLSL boolean values.
  And :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
  -- | Compute the logical disjunction of two GLSL boolean values.
  Or :: Expr GLboolean -> Expr GLboolean -> Expr GLboolean
  -- | Select one of two values based on a GLSL boolean value.
  Selection :: (Typed x) => Expr GLboolean -> Expr x -> Expr x -> Expr x

instance (R1 v, Typed e) => HasField "x" (Expr (v e)) (Expr e) where
  getField = FieldSelection "x"

instance (R2 v, Typed e) => HasField "y" (Expr (v e)) (Expr e) where
  getField = FieldSelection "y"

instance (R3 v, Typed e) => HasField "z" (Expr (v e)) (Expr e) where
  getField = FieldSelection "z"

instance (R4 v, Typed e) => HasField "w" (Expr (v e)) (Expr e) where
  getField = FieldSelection "w"

-- | Convert a Haskell expression into a GLSL abstract syntax tree. This
-- performs no type-checking and is extremely naïve, so optimisations and AST
-- passes should be done before this call.
toGLSL :: Expr x -> Syntax.Expr
toGLSL = \case
  IntConstant x -> Syntax.IntConstant Syntax.Decimal x
  FloatConstant x -> Syntax.FloatConstant x
  BoolConstant x -> Syntax.BoolConstant x
  Cast x -> toGLSL x
  Add x y -> Syntax.Add (toGLSL x) (toGLSL y)
  And x y -> Syntax.And (toGLSL x) (toGLSL y)
  Or x y -> Syntax.Or (toGLSL x) (toGLSL y)
  FieldSelection s x -> Syntax.FieldSelection (toGLSL x) s
  FunctionCall f x -> Syntax.FunctionCall (Syntax.FuncId f) do
    Syntax.Params (map (Some.foldSome toGLSL) x)
  Selection p x y -> Syntax.Selection (toGLSL p) (toGLSL x) (toGLSL y)
