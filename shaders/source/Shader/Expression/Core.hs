{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Trans.Cofree qualified as CofreeF
import Data.Functor.Foldable (cata)
import Data.Kind (Type)
import GHC.Records (HasField (getField))
import Language.GLSL.Syntax qualified as Syntax
import Linear (R1, R2, R3, R4)
import Shader.Expression.Type (Typed (typeOf))

-- | An expression is a GLSL value that has a type. We keep track of the type
-- of the expression and all its subexpressions in case we want to factor them
-- out as intermediate bindings.
type Expr :: Type -> Type
newtype Expr x = Expr {toAST :: Cofree Expression Syntax.TypeSpecifier}

-- | A convenience function for lifting an expression into 'Expr'.
expr_ :: forall x. (Typed x) => Expression (Cofree Expression Syntax.TypeSpecifier) -> Expr x
expr_ = Expr . \xs -> typeOf @x :< xs

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
  | -- | Implicit GLSL type conversion.
    Cast inner
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

instance (R1 v, Typed e) => HasField "x" (Expr (v e)) (Expr e) where
  getField = expr_ . FieldSelection "x" . toAST

instance (R2 v, Typed e) => HasField "y" (Expr (v e)) (Expr e) where
  getField = expr_ . FieldSelection "y" . toAST

instance (R3 v, Typed e) => HasField "z" (Expr (v e)) (Expr e) where
  getField = expr_ . FieldSelection "z" . toAST

instance (R4 v, Typed e) => HasField "w" (Expr (v e)) (Expr e) where
  getField = expr_ . FieldSelection "w" . toAST

-- | Convert a Haskell expression into a GLSL abstract syntax tree. This
-- performs no type-checking and is extremely naïve, so optimisations and AST
-- passes should be done before this call.
toGLSL :: Expr x -> Syntax.Expr
toGLSL = go . toAST
  where
    go :: Cofree Expression Syntax.TypeSpecifier -> Syntax.Expr
    go = cata \(_ CofreeF.:< expr) -> case expr of
      BoolConstant x -> Syntax.BoolConstant x
      FloatConstant x -> Syntax.FloatConstant x
      IntConstant x -> Syntax.IntConstant Syntax.Decimal x
      Cast x -> x
      Add x y -> Syntax.Add x y
      And x y -> Syntax.And x y
      Or x y -> Syntax.Or x y
      FieldSelection s xs -> Syntax.FieldSelection xs s
      FunctionCall f xs -> Syntax.FunctionCall (Syntax.FuncId f) (Syntax.Params xs)
      Selection p x y -> Syntax.Selection p x y
