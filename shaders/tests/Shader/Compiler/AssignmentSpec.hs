{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Shader.Compiler.AssignmentSpec where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Some (foldSome)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLint)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Shader.Compiler.Assignment (Assignment (assignments))
import Shader.Expression (Expr, lift)
import Shader.Expression.Core (Expr (Expr, unExpr))
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)

type Coord :: Type
data Coord = Coord
  { x :: Expr GLint,
    y :: Expr GLint,
    z :: Maybe (Expr GLint)
  }
  deriving stock (Generic)
  deriving anyclass (Assignment)

spec :: Spec
spec = do
  it "all present" $ hedgehog do
    x <- forAll Gen.enumBounded
    y <- forAll Gen.enumBounded
    z <- forAll Gen.enumBounded

    let coord :: Coord
        coord = Coord {x = lift x, y = lift y, z = Just (lift z)}

    fmap (foldSome (Expr . unExpr)) (assignments coord)
      === Map.fromList [("x", lift x), ("y", lift y), ("z", lift z)]

  it "missing value" $ hedgehog do
    x <- forAll Gen.enumBounded
    y <- forAll Gen.enumBounded

    let coord :: Coord
        coord = Coord {x = lift x, y = lift y, z = Nothing}

    fmap (foldSome (Expr . unExpr)) (assignments coord)
      === Map.fromList [("x", lift x), ("y", lift y)]
