{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}

module Shader.Expression.LogicSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLboolean)
import Hedgehog (Gen, annotateShow, evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly)
import Linear (V1 (V1), V4 (V4))
import Shader.Expression (Expr, bvec4, false, ifThenElse, lift, true, (&&), (||))
import Test.Hspec (SpecWith, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding ((&&), (||))

genBoolean :: Gen GLboolean
genBoolean = Gen.element [1, 0]

spec :: SpecWith Renderer
spec = do
  it "selection" \renderer -> hedgehog do
    p <- forAll genBoolean

    x <- forAll genZeroToOne
    y <- forAll genZeroToOne

    let expr = if lift p then lift x else lift y
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    V1 output `isRoughly` case p of 1 -> V1 x; _ -> V1 y

  let test :: (Expr GLboolean -> Expr GLboolean -> Expr GLboolean) -> Expr (V4 GLboolean)
      test f = bvec4 (g false false) (g false true) (g true false) (g true true)
        where
          g x y = if f x y then lift 1 else lift 0

  it "conjunction" \renderer -> do
    output <- liftIO (renderExpr renderer (test (&&)))
    output `shouldBe` V4 0 0 0 1

  it "disjunction" \renderer -> do
    output <- liftIO (renderExpr renderer (test (||)))
    output `shouldBe` V4 0 1 1 1
