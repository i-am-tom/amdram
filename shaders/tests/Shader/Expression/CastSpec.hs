{-# LANGUAGE BlockArguments #-}

module Shader.Expression.CastSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V4 (V4))
import Shader.Expression (cast, ivec4, lift, vec4)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: SpecWith Renderer
spec = do
  it "int -> float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.float (Range.linearFrac 0 1)
    z <- forAll $ Gen.float (Range.linearFrac 0 1)

    output <- liftIO $ renderExpr renderer do
      let x' = cast @GLint @GLfloat (lift x)
      vec4 x' (lift y) (lift z) (lift 1)

    V4 (fromIntegral x) y z 1 `isRoughly` output

  it "V4 int -> V4 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      cast (ivec4 (lift x) (lift y) (lift z) (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output
