{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Shader.Compiler.AssignmentSpec where

import Control.Comonad.Cofree (Cofree)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Some (Some)
import Data.Some qualified as Some
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.GLSL.Syntax (TypeSpecifier)
import Shader.Compiler.Assignment (Assignment (assignments))
import Shader.Expression (Expr, lift)
import Shader.Expression.Core (ExprF, unExpr)
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)

type Coord :: Type
data Coord = Coord
  { x :: Maybe (Expr GLfloat),
    y :: Expr GLfloat,
    z :: Expr GLfloat
  }
  deriving stock (Generic)
  deriving anyclass (Assignment)

spec :: Spec
spec = do
  it "generic assignments" $ hedgehog do
    let float :: Gen (Expr GLfloat)
        float = fmap lift $ Gen.float do
          Range.linearFrac 0 100

    mx <- forAll (Gen.maybe float)
    y@(unExpr -> ry) <- forAll float
    z@(unExpr -> rz) <- forAll float

    let result :: Map String (Some Expr)
        result = assignments Coord {x = mx, y = y, z = z}

        mxs :: Map String (Cofree ExprF TypeSpecifier)
        mxs = case mx of
          Just x -> Map.singleton "x" (unExpr x)
          Nothing -> Map.empty

    fmap (Some.foldSome unExpr) result === mxs <> [("y", ry), ("z", rz)]
