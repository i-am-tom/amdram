{-# LANGUAGE BlockArguments #-}

module Shader.Expression.ConstantsSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Hedgehog (Gen, MonadTest, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V4 (V4))
import Shader.Expression.Constants (fromInteger, fromRational, lift)
import Shader.Expression.Core (Expr, vec4)
import Test.Hspec (SpecWith, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)
import Prelude qualified

genZeroToOne :: Gen Float
genZeroToOne = Gen.float (Range.linearFrac 0 1)

spec :: SpecWith Renderer
spec = do
  it "lift @GLfloat" \renderer -> hedgehog do
    input@(V4 x y z w) <- forAll do
      x <- genZeroToOne
      y <- genZeroToOne
      z <- genZeroToOne

      pure $ V4 @GLfloat x y z 1

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift w)

    input `isRoughly` output

  it "lift @(V4 GLfloat)" \renderer -> hedgehog do
    input <- forAll do
      x <- genZeroToOne
      y <- genZeroToOne
      z <- genZeroToOne

      pure $ V4 @GLfloat x y z 1

    output <- liftIO $ renderExpr renderer (lift input)
    input `isRoughly` output
