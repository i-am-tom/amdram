{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.CoreSpec where

import Hedgehog (annotateShow, evalIO, forAll)
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

    let expr = let i = lift (V4 x y z 1) in vec4 i.x i.y i.z i.w
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V4 x y z 1
