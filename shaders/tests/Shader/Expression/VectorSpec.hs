{-# LANGUAGE BlockArguments #-}

module Shader.Expression.VectorSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat)
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (shouldRoughlyBe)
import Linear (V4 (V4))
import Shader.Expression.Constants (Lift (lift))
import Shader.Expression.Vector (vec2, vec3, vec4)
import Test.Hspec (SpecWith, it)
import Prelude hiding (fromInteger, fromRational)

-- We specifically keep 'lift' polymorphic in here to give GHC no help with
-- type inference. The correct instance for 'Lift' should be deduced from the
-- fact that it's being used as a scalar component to build a 'GLfloat' vector.
spec :: SpecWith Renderer
spec = do
  let x :: GLfloat
      x = 0.25

      y :: GLfloat
      y = 0.5

      z :: GLfloat
      z = 0.75

      w :: GLfloat
      w = 1

  it "vec4" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (vec4 (lift x, lift y, lift z, lift w))
    V4 x y z w `shouldRoughlyBe` output

  it "1 + vec3" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (lift x, vec3 (lift y, lift z, lift w))
    V4 x y z w `shouldRoughlyBe` output

  it "vec3 + 1" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (vec3 (lift x, lift y, lift z), lift w)
    V4 x y z w `shouldRoughlyBe` output

  it "vec2 + 1 + 1" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (vec2 (lift x, lift y), lift z, lift w)
    V4 x y z w `shouldRoughlyBe` output

  it "1 + vec2 + 1" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (lift x, vec2 (lift y, lift z), lift w)
    V4 x y z w `shouldRoughlyBe` output

  it "1 + 1 + vec2" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (lift x, lift y, vec2 (lift z, lift w))
    V4 x y z w `shouldRoughlyBe` output

  it "vec2 + vec2" \renderer -> do
    output <- liftIO $ renderExpr renderer do
      vec4 (vec2 (lift x, lift y), vec2 (lift z, lift w))
    V4 x y z w `shouldRoughlyBe` output
