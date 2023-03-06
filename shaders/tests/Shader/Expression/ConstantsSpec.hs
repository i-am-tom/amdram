{-# LANGUAGE BlockArguments #-}

module Shader.Expression.ConstantsSpec where

import Graphics.Rendering.OpenGL (GLboolean, GLint)
import Hedgehog (Gen, annotateShow, evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V1 (V1), V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (lift)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

genZeroToOne :: Gen Float
genZeroToOne = Gen.float (Range.linearFrac 0 1)

genBoolean :: Gen GLboolean
genBoolean = Gen.element [1, 0]

spec :: SpecWith Renderer
spec = do
  it "lift @GLboolean" \renderer -> hedgehog do
    x <- forAll genBoolean

    let expr = lift x
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    V1 output === V1 x

  it "lift @(V2 GLboolean)" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean

    let expr = lift (V2 x y)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output === V2 x y

  it "lift @(V3 GLboolean)" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean
    z <- forAll genBoolean

    let expr = lift (V3 x y z)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output === V3 x y z

  it "lift @(V4 GLboolean)" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean
    z <- forAll genBoolean

    let expr = lift (V4 x y z 1)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output === V4 x y z 1

  it "lift @GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToOne

    let expr = lift x
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    V1 output `isRoughly` V1 x

  it "lift @(V2 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne

    let expr = lift (V2 x y)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V2 x y

  it "lift @(V3 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    let expr = lift (V3 x y z)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V3 x y z

  it "lift @(V4 GLfloat)" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    let expr = lift (V4 x y z 1)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V4 x y z 1

  it "lift @GLint" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])

    let expr = lift x
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    V1 output === V1 x

  it "lift @(V2 GLint)" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])
    y :: GLint <- forAll (Gen.element [0, 1])

    let expr = lift (V2 x y)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output === V2 x y

  it "lift @(V3 GLint)" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])
    y :: GLint <- forAll (Gen.element [0, 1])
    z :: GLint <- forAll (Gen.element [0, 1])

    let expr = lift (V3 x y z)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output === V3 x y z

  it "lift @(V4 GLint)" \renderer -> hedgehog do
    x :: GLint <- forAll (Gen.element [0, 1])
    y :: GLint <- forAll (Gen.element [0, 1])
    z :: GLint <- forAll (Gen.element [0, 1])

    let expr = lift (V4 x y z 1)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output === V4 x y z 1
