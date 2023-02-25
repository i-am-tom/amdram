{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.ConstantsSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Hedgehog (Gen, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (Expr, cast, ivec4, lift, vec4)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

genZeroToOne :: Gen Float
genZeroToOne = Gen.float (Range.linearFrac 0 1)

spec :: SpecWith Renderer
spec = do
  it "lift @GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output

  it "lift @(V2 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLfloat)
          partial = lift (V2 x y)

      vec4 partial.x partial.y (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output

  it "lift @(V3 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLfloat)
          partial = lift (V3 x y z)

      vec4 partial.x partial.y partial.z (lift 1)

    V4 x y z 1 `isRoughly` output

  it "lift @(V4 GLfloat)" \renderer -> hedgehog do
    input <- forAll do
      x <- genZeroToOne
      y <- genZeroToOne
      z <- genZeroToOne

      pure $ V4 @GLfloat x y z 1

    output <- liftIO $ renderExpr renderer (lift input)
    input `isRoughly` output

  it "lift @GLint" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      cast (ivec4 (lift x) (lift y) (lift z) (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output

  it "lift @(V2 GLint)" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLint)
          partial = lift (V2 x y)

      cast (ivec4 partial.x partial.y (lift z) (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output

  it "lift @(V3 GLint)" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLint)
          partial = lift (V3 x y z)

      cast (ivec4 partial.x partial.y partial.z (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output

  it "lift @(V4 GLint)" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      cast @(V4 GLint) (lift (V4 x y z 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output
