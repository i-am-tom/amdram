{-# LANGUAGE BlockArguments #-}

module Shader.Expression.VectorSpec where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (forAll)
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly)
import Linear (V4 (V4))
import Shader.Expression.Constants (Lift (lift))
import Shader.Expression.Vector (vec4)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

spec :: SpecWith Renderer
spec = do
  it "vec4" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output
