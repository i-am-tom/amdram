{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}

module Shader.Expression.LogicSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import GHC.Records (getField)
import Graphics.Rendering.OpenGL (GLboolean, GLfloat)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly, shouldRoughlyBe)
import Linear (V3 (V3), V4 (V4))
import Shader.Expression (Expr, false, ifThenElse, lift, true, vec4, (&&), (||))
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding ((&&), (||))

spec :: SpecWith Renderer
spec = do
  it "selection" \renderer -> hedgehog do
    p <- forAll Gen.bool

    a <- forAll genZeroToOne
    b <- forAll genZeroToOne
    c <- forAll genZeroToOne

    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let check :: Expr GLboolean
          check = bool false true p

          partial :: Expr (V3 GLfloat)
          partial =
            if check
              then lift (V3 a b c)
              else lift (V3 x y z)

      vec4 partial.x partial.y partial.z (lift 1)

    output `isRoughly` bool (V4 x y z 1) (V4 a b c 1) p

  it "conjunction" \renderer -> do
    let x :: Expr GLfloat
        x = if false && false then lift 1 else lift 0

        y :: Expr GLfloat
        y = if false && true then lift 1 else lift 0

        z :: Expr GLfloat
        z = if true && false then lift 1 else lift 0

        w :: Expr GLfloat
        w = if true && true then lift 1 else lift 0

    output <- liftIO $ renderExpr renderer (vec4 x y z w)
    V4 0 0 0 1 `shouldRoughlyBe` output

  it "disjunction" \renderer -> do
    let x :: Expr GLfloat
        x = if false || false then lift 1 else lift 0

        y :: Expr GLfloat
        y = if false || true then lift 1 else lift 0

        z :: Expr GLfloat
        z = if true || false then lift 1 else lift 0

        w :: Expr GLfloat
        w = if true || true then lift 1 else lift 0

    output <- liftIO $ renderExpr renderer (vec4 x y z w)
    V4 0 1 1 1 `shouldRoughlyBe` output
