{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.VectorSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Graphics.Rendering.OpenGL (GLboolean, GLfloat, GLint)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly)
import Linear (V2, V3, V4 (V4))
import Shader.Expression (Expr, cast, lift)
import Shader.Expression qualified as Expr
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

spec :: SpecWith Renderer
spec = do
  let bool2gl :: Bool -> Expr GLboolean
      bool2gl = bool Expr.false Expr.true

      bool2float :: Bool -> Float
      bool2float = bool 0 1

      enum :: Expr GLboolean -> Expr GLfloat
      enum p = Expr.ifThenElse p (lift 1) (lift 0)

  it "bvec2" \renderer -> hedgehog do
    x <- forAll Gen.bool
    y <- forAll Gen.bool

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLboolean)
          partial = Expr.bvec2 (bool2gl x) (bool2gl y)

      Expr.vec4 (enum partial.x) (enum partial.y) (lift 1) (lift 1)

    V4 (bool2float x) (bool2float y) 1 1 `isRoughly` output

  it "bvec3" \renderer -> hedgehog do
    x <- forAll Gen.bool
    y <- forAll Gen.bool
    z <- forAll Gen.bool

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLboolean)
          partial = Expr.bvec3 (bool2gl x) (bool2gl y) (bool2gl z)

      Expr.vec4 (enum partial.x) (enum partial.y) (enum partial.z) (lift 1)

    V4 (bool2float x) (bool2float y) (bool2float z) 1 `isRoughly` output

  it "bvec4" \renderer -> hedgehog do
    x <- forAll Gen.bool
    y <- forAll Gen.bool
    z <- forAll Gen.bool

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V4 GLboolean)
          partial = Expr.bvec4 (bool2gl x) (bool2gl y) (bool2gl z) (bool2gl True)

      Expr.vec4 (enum partial.x) (enum partial.y) (enum partial.z) (lift 1)

    V4 (bool2float x) (bool2float y) (bool2float z) 1 `isRoughly` output

  it "ivec2" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.float (Range.linearFrac 0 1)

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLint)
          partial = Expr.ivec2 (lift x) (lift y)

      Expr.vec4 (cast partial.x) (cast partial.y) (lift z) (lift 1)

    V4 (fromIntegral x) (fromIntegral y) z 1 `isRoughly` output

  it "ivec3" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLint)
          partial = Expr.ivec3 (lift x) (lift y) (lift z)

      Expr.vec4 (cast partial.x) (cast partial.y) (cast partial.z) (lift 1)

    fmap fromIntegral (V4 x y z 1) `isRoughly` output

  it "ivec4" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      cast (Expr.ivec4 (lift x) (lift y) (lift z) (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output

  it "vec2" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V2 GLfloat)
          partial = Expr.vec2 (lift x) (lift y)

      Expr.vec4 partial.x partial.y (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output

  it "vec3" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let partial :: Expr (V3 GLfloat)
          partial = Expr.vec3 (lift x) (lift y) (lift z)

      Expr.vec4 partial.x partial.y partial.z (lift 1)

    V4 x y z 1 `isRoughly` output

  it "vec4" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      Expr.vec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output
