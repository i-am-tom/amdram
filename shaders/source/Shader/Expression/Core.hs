{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Typed syntactic sugar on top of @language-glsl@.
module Shader.Expression.Core
  ( Expr (..),
    toGLSL,
    ExprF (..),
    unsafeLift,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Cofree.Extra (decapitate)
import Control.Comonad.Trans.Cofree (CofreeF)
import Data.Functor.Classes (Eq1 (liftEq), Show1 (liftShowsPrec))
import Data.Functor.Classes.Generic (liftEqDefault, liftShowsPrecDefault)
import Data.Functor.Foldable (cata, project)
import Data.Kind (Type)
import Data.Reify (MuRef (DeRef, mapDeRef))
import GHC.Generics (Generic1)
import GHC.Records (HasField (getField))
import Language.GLSL.Syntax qualified as Syntax
import Linear (R1, R2, R3, R4)
import Shader.Expression.Type (Typed (typeOf))

-- | An expression is a GLSL value that has a type. We keep track of the type
-- of the expression and all its subexpressions in case we want to factor them
-- out as intermediate bindings.
type Expr :: Type -> Type
newtype Expr x = Expr {unExpr :: Cofree ExprF Syntax.TypeSpecifier}
  deriving newtype (Eq, Show)

-- | The inner data type for 'Expr'. These constructors should map in very
-- straightforward ways to 'Syntax.Expr'. We express this as a separate functor
-- so that we can both fold it /and/ reuse it for generating intermediate
-- binds.
type ExprF :: Type -> Type
data ExprF expr
  = IntConstant Integer
  | FloatConstant Float
  | BoolConstant Bool
  | Cast expr
  | FieldSelection String expr
  | FunctionCall String [expr]
  | Add expr expr
  | And expr expr
  | Or expr expr
  | Selection expr expr expr
  deriving stock (Eq, Generic1, Ord, Show)
  deriving stock (Foldable, Functor, Traversable)

instance Eq1 ExprF where
  liftEq = liftEqDefault

instance Show1 ExprF where
  liftShowsPrec = liftShowsPrecDefault

instance MuRef (Cofree ExprF x) where
  type DeRef (Cofree ExprF x) = CofreeF ExprF x
  mapDeRef f = traverse f . project

-- | Lift a chunk of AST into an 'Expr' with the given type. This is a very
-- unsafe function as there's no guarantees about type correctness, and is only
-- here as a convenience function for internnal definitions.
unsafeLift :: forall x. (Typed x) => ExprF (Cofree ExprF Syntax.TypeSpecifier) -> Expr x
unsafeLift xs = Expr (typeOf @x :< xs)

instance (R1 v, Typed e) => HasField "x" (Expr (v e)) (Expr e) where
  getField (Expr xs) = unsafeLift (FieldSelection "x" xs)

instance (R2 v, Typed e) => HasField "y" (Expr (v e)) (Expr e) where
  getField (Expr xs) = unsafeLift (FieldSelection "y" xs)

instance (R3 v, Typed e) => HasField "z" (Expr (v e)) (Expr e) where
  getField (Expr xs) = unsafeLift (FieldSelection "z" xs)

instance (R4 v, Typed e) => HasField "w" (Expr (v e)) (Expr e) where
  getField (Expr xs) = unsafeLift (FieldSelection "w" xs)

-- | Convert a Haskell expression into a GLSL abstract syntax tree. This
-- performs no type-checking and is extremely naïve, so optimisations and AST
-- passes should be done before this call.
toGLSL :: Expr x -> Syntax.Expr
toGLSL (Expr expr) = flip cata (decapitate expr) \case
  IntConstant x -> Syntax.IntConstant Syntax.Decimal x
  FloatConstant x -> Syntax.FloatConstant x
  BoolConstant x -> Syntax.BoolConstant x
  Cast x -> x
  Add x y -> Syntax.Add x y
  And x y -> Syntax.And x y
  Or x y -> Syntax.Or x y
  FieldSelection s x -> Syntax.FieldSelection x s
  FunctionCall f xs -> Syntax.FunctionCall (Syntax.FuncId f) (Syntax.Params xs)
  Selection p x y -> Syntax.Selection p x y
