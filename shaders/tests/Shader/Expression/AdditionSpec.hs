{-# LANGUAGE BlockArguments #-}

module Shader.Expression.AdditionSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog (Gen, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V1 (V1), V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (lift, vec2, vec3, vec4, (+))
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding ((+))
import Prelude qualified

genZeroToHalf :: Gen GLfloat
genZeroToHalf = Gen.float (Range.linearFrac 0 0.5)

spec :: SpecWith Renderer
spec = do
  it "GLfloat + GLfloat = GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer (lift x + lift y)
    V1 (x Prelude.+ y) `isRoughly` V1 output

  it "V2 GLfloat + GLfloat = V2 GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    r <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      vec2 (lift x) (lift y) + lift r

    V2 (x Prelude.+ r) (y Prelude.+ r) `isRoughly` output

  it "GLfloat + V2 GLfloat = V2 GLfloat" \renderer -> hedgehog do
    r <- forAll genZeroToHalf
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      lift r + vec2 (lift x) (lift y)

    V2 (x Prelude.+ r) (y Prelude.+ r) `isRoughly` output

  it "V2 GLfloat + V2 GLfloat = V2 GLfloat" \renderer -> hedgehog do
    a <- forAll genZeroToHalf
    b <- forAll genZeroToHalf
    c <- forAll genZeroToHalf
    d <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      vec2 (lift a) (lift b) + vec2 (lift c) (lift d)

    V2 (a Prelude.+ c) (b Prelude.+ d) `isRoughly` output

  it "V3 GLfloat + GLfloat = V3 GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf
    r <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      vec3 (lift x) (lift y) (lift z) + lift r

    V3 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) `isRoughly` output

  it "GLfloat + V3 GLfloat = V3 GLfloat" \renderer -> hedgehog do
    r <- forAll genZeroToHalf
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      lift r + vec3 (lift x) (lift y) (lift z)

    V3 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) `isRoughly` output

  it "V3 GLfloat + V3 GLfloat = V3 GLfloat" \renderer -> hedgehog do
    a <- forAll genZeroToHalf
    b <- forAll genZeroToHalf
    c <- forAll genZeroToHalf
    d <- forAll genZeroToHalf
    e <- forAll genZeroToHalf
    f <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      vec3 (lift a) (lift b) (lift c) + vec3 (lift d) (lift e) (lift f)

    V3 (a Prelude.+ d) (b Prelude.+ e) (c Prelude.+ f) `isRoughly` output

  it "V4 GLfloat + GLfloat = V4 GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf
    r <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift (1 - r)) + lift r

    V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1 `isRoughly` output

  it "GLfloat + V4 GLfloat = V4 GLfloat" \renderer -> hedgehog do
    r <- forAll genZeroToHalf
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      lift r + vec4 (lift x) (lift y) (lift z) (lift (1 - r))

    V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1 `isRoughly` output

  it "V4 GLfloat + V4 GLfloat = V4 GLfloat" \renderer -> hedgehog do
    a <- forAll genZeroToHalf
    b <- forAll genZeroToHalf
    c <- forAll genZeroToHalf
    d <- forAll genZeroToHalf
    e <- forAll genZeroToHalf
    f <- forAll genZeroToHalf

    output <- liftIO $ renderExpr renderer do
      vec4 (lift a) (lift b) (lift c) (lift 0.5)
        + vec4 (lift d) (lift e) (lift f) (lift 0.5)

    V4 (a Prelude.+ d) (b Prelude.+ e) (c Prelude.+ f) 1 `isRoughly` output
