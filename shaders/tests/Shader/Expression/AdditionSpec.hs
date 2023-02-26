{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.AdditionSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V2, V3, V4 (V4))
import Shader.Expression (Expr, lift, vec2, vec3, vec4, (+))
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding ((+))
import Prelude qualified

spec :: SpecWith Renderer
spec = do
  it "GLfloat + GLfloat = GLfloat" \renderer -> hedgehog do
    x1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x1 + lift x2) (lift y) (lift z) (lift 1)

    V4 (x1 Prelude.+ x2) y z 1 `isRoughly` output

  it "V2 GLfloat + GLfloat = V2 GLfloat" \renderer -> hedgehog do
    x <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    r <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLfloat)
          partial = vec2 (lift x) (lift y) + lift r

      vec4 partial.x partial.y (lift z) (lift 1)

    V4 (x Prelude.+ r) (y Prelude.+ r) z 1 `isRoughly` output

  it "GLfloat + V2 GLfloat = V2 GLfloat" \renderer -> hedgehog do
    r <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLfloat)
          partial = lift r + vec2 (lift x) (lift y)

      vec4 partial.x partial.y (lift z) (lift 1)

    V4 (x Prelude.+ r) (y Prelude.+ r) z 1 `isRoughly` output

  it "V2 GLfloat + V2 GLfloat = V2 GLfloat" \renderer -> hedgehog do
    x1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLfloat)
          partial = vec2 (lift x1) (lift y1) + vec2 (lift x2) (lift y2)

      vec4 partial.x partial.y (lift z) (lift 1)

    V4 (x1 Prelude.+ x2) (y1 Prelude.+ y2) z 1 `isRoughly` output

  it "V3 GLfloat + GLfloat = V3 GLfloat" \renderer -> hedgehog do
    x <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    r <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLfloat)
          partial = vec3 (lift x) (lift y) (lift z) + lift r

      vec4 partial.x partial.y partial.z (lift 1)

    V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1 `isRoughly` output

  it "GLfloat + V3 GLfloat = V3 GLfloat" \renderer -> hedgehog do
    r <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLfloat)
          partial = lift r + vec3 (lift x) (lift y) (lift z)

      vec4 partial.x partial.y partial.z (lift 1)

    V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1 `isRoughly` output

  it "V3 GLfloat + V3 GLfloat = V3 GLfloat" \renderer -> hedgehog do
    x1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLfloat)
          partial =
            vec3 (lift x1) (lift y1) (lift z1)
              + vec3 (lift x2) (lift y2) (lift z2)

      vec4 partial.x partial.y partial.z (lift 1)

    V4 (x1 Prelude.+ x2) (y1 Prelude.+ y2) (z1 Prelude.+ z2) 1 `isRoughly` output

  it "V4 GLfloat + GLfloat = V4 GLfloat" \renderer -> hedgehog do
    x <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    r <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift (1 - r)) + lift r

    V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1 `isRoughly` output

  it "GLfloat + V4 GLfloat = V4 GLfloat" \renderer -> hedgehog do
    r <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      lift r + vec4 (lift x) (lift y) (lift z) (lift (1 - r))

    V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1 `isRoughly` output

  it "V4 GLfloat + V4 GLfloat = V4 GLfloat" \renderer -> hedgehog do
    x1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    x2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    y2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z1 <- forAll $ Gen.float (Range.linearFrac 0 0.5)
    z2 <- forAll $ Gen.float (Range.linearFrac 0 0.5)

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x1) (lift y1) (lift z1) (lift 0.5)
        + vec4 (lift x2) (lift y2) (lift z2) (lift 0.5)

    V4 (x1 Prelude.+ x2) (y1 Prelude.+ y2) (z1 Prelude.+ z2) 1 `isRoughly` output
