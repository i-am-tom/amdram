{-# LANGUAGE BlockArguments #-}

module Shader.Expression.ConstantsSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLboolean, GLint)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V1 (V1), V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (lift)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

genZeroToOne :: Gen Float
genZeroToOne = Gen.float (Range.linearFrac 0 1)

genBoolean :: Gen GLboolean
genBoolean = Gen.element [1, 0]

spec :: SpecWith Renderer
spec = do
  it "lift @GLboolean" \renderer -> hedgehog do
    x <- forAll genBoolean

    output <- liftIO $ renderExpr renderer (lift x)
    V1 x === V1 output

  it "lift @(V2 GLboolean)" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean

    output <- liftIO $ renderExpr renderer do
      lift (V2 x y)

    V2 x y === output

  it "lift @(V3 GLboolean)" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean
    z <- forAll genBoolean

    output <- liftIO $ renderExpr renderer do
      lift (V3 x y z)

    V3 x y z === output

  it "lift @(V4 GLboolean)" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean
    z <- forAll genBoolean

    output <- liftIO $ renderExpr renderer do
      lift (V4 x y z 1)

    V4 x y z 1 === output

  it "lift @GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer (lift x)
    V1 x `isRoughly` V1 output

  it "lift @(V2 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      lift (V2 x y)

    V2 x y `isRoughly` output

  it "lift @(V3 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      lift (V3 x y z)

    V3 x y z `isRoughly` output

  it "lift @(V4 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      lift (V4 x y z 1)

    V4 x y z 1 `isRoughly` output

  it "lift @GLint" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])

    output <- liftIO $ renderExpr renderer (lift x)
    V1 x === V1 output

  it "lift @(V2 GLint)" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])
    y :: GLint <- forAll (Gen.element [0, 1])

    output <- liftIO $ renderExpr renderer do
      lift (V2 x y)

    V2 x y === output

  it "lift @(V3 GLint)" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])
    y :: GLint <- forAll (Gen.element [0, 1])
    z :: GLint <- forAll (Gen.element [0, 1])

    output <- liftIO $ renderExpr renderer do
      lift (V3 x y z)

    V3 x y z === output

  it "lift @(V4 GLint)" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])
    y :: GLint <- forAll (Gen.element [0, 1])
    z :: GLint <- forAll (Gen.element [0, 1])

    output <- liftIO $ renderExpr renderer do
      lift (V4 x y z 1)

    V4 x y z 1 === output
