{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression.Core
  ( Expr,
    expr_,
    Expression (..),
    toAST,
    toGLSL,
  )
where

import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (cata)
import Data.Kind (Type)
import GHC.Records (HasField (getField))
import Language.GLSL.Syntax qualified as Syntax
import Linear (R1, R2, R3, R4)

-- | An expression is a GLSL value that has a type. In practice, because all
-- computation will be performed on the GPU once we've compiled the shader, the
-- type here is entirely phantom - it's just there to make the operations
-- well typed.
type Expr :: Type -> Type
newtype Expr x = Expr {toAST :: Fix Expression}

-- | A convenience function for lifting an expression into 'Expr'.
expr_ :: Expression (Fix Expression) -> Expr x
expr_ = Expr . Fix

-- | We define the expression language using the 'Fix' point of this functor.
-- This allows us to annotate the AST later with other annotations such as
-- types.
type Expression :: Type -> Type
data Expression inner
  = -- | A constant value displayed as an integer. This could be an @int@, but
    -- it could also be a @uint@ or any other type that can be given as an
    -- integral constant.
    IntConstant Integer
  | -- | A constant vvalue displayed as a floating point number. This should
    -- either be a @float@ or a @double@.
    FloatConstant Float
  | -- | A boolean value of type @boolean@.
    BoolConstant Bool
  | -- | A single field within a vector, or a swizzling mask.
    FieldSelection String inner
  | -- | A call to the given function name with the given arguments.
    FunctionCall String [inner]
  | -- | Arithmetic sum, which compiles to the GLSL @+@.
    Add inner inner
  | -- | Logical AND operation, which compiles to the GLSL @&&@.
    And inner inner
  | -- | Logical OR operation, which compiles to the GLSL @&&@.
    Or inner inner
  | -- | Boolean selection, which is represented in GLSL with @?@ and @:@. Maps
    -- exactly to Haskell's @ifThenElse@ construction.
    Selection inner inner inner
  deriving stock (Eq, Ord, Functor, Show)

instance (R1 v) => HasField "x" (Expr (v e)) (Expr e) where
  getField = Expr . Fix . FieldSelection "x" . toAST

instance (R2 v) => HasField "y" (Expr (v e)) (Expr e) where
  getField = Expr . Fix . FieldSelection "y" . toAST

instance (R3 v) => HasField "z" (Expr (v e)) (Expr e) where
  getField = Expr . Fix . FieldSelection "z" . toAST

instance (R4 v) => HasField "w" (Expr (v e)) (Expr e) where
  getField = Expr . Fix . FieldSelection "w" . toAST

-- | Convert a Haskell expression into a GLSL abstract syntax tree. This
-- performs no type-checking and is extremely naïve, so optimisations and AST
-- passes should be done before this call.
toGLSL :: Expr x -> Syntax.Expr
toGLSL = go . toAST
  where
    go :: Fix Expression -> Syntax.Expr
    go = cata \case
      BoolConstant x -> Syntax.BoolConstant x
      FloatConstant x -> Syntax.FloatConstant x
      IntConstant x -> Syntax.IntConstant Syntax.Decimal x
      Add x y -> Syntax.Add x y
      And x y -> Syntax.And x y
      Or x y -> Syntax.Or x y
      FieldSelection s xs -> Syntax.FieldSelection xs s
      FunctionCall f xs -> Syntax.FunctionCall (Syntax.FuncId f) (Syntax.Params xs)
      Selection p x y -> Syntax.Selection p x y
