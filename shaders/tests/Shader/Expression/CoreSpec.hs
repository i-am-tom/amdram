{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.CoreSpec where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (forAll)
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly)
import Linear (V4 (V4))
import Shader.Expression (lift, vec4)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: SpecWith Renderer
spec = do
  it "component selection" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let input = lift (V4 x y z 1)
      vec4 input.x input.y input.z input.w

    V4 x y z 1 `isRoughly` output
