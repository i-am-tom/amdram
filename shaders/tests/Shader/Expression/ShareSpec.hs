{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.ShareSpec where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (find)
import Data.Graph (Vertex)
import Data.Reify (Unique)
import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog (Gen, MonadTest, annotateShow, failure, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer)
import Language.GLSL.Syntax (TypeSpecifier)
import Shader.Expression (lift, share, vec2, vec3, vec4)
import Shader.Expression.Core (Expr (Expr, unExpr), ExprF)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)

genShader :: Gen (Expr GLfloat)
genShader = do
  let genFloat :: Gen (Expr GLfloat)
      genFloat = fmap lift do
        Gen.float (Range.linearFrac 0 10)

      genVec2 :: Gen (Expr GLfloat)
      genVec2 = do
        x <- genFloat
        y <- genFloat

        Gen.element
          [ (vec2 x y).x,
            (vec2 x y).y
          ]

      genVec3 :: Gen (Expr GLfloat)
      genVec3 = do
        x <- genFloat
        y <- genFloat
        z <- genFloat

        Gen.element
          [ (vec3 x y z).x,
            (vec3 x y z).y,
            (vec3 x y z).z
          ]

      genVec4 :: Gen (Expr GLfloat)
      genVec4 = do
        x <- genFloat
        y <- genFloat
        z <- genFloat
        w <- genFloat

        Gen.element
          [ (vec4 x y z w).x,
            (vec4 x y z w).y,
            (vec4 x y z w).z,
            (vec4 x y z w).w
          ]

  Gen.recursive Gen.choice [genFloat] [genVec2, genVec3, genVec4]

spec :: SpecWith Renderer
spec = do
  it "roundtrips" \_ -> hedgehog do
    input <- forAll genShader

    shared <- liftIO (share input)
    annotateShow shared

    let search :: (MonadTest m) => Vertex -> [(Unique, x, y)] -> m (x, y)
        search index =
          maybe failure (pure . removeIndex)
            . find \(check, _, _) -> index == check
          where
            removeIndex :: (x, y, z) -> (y, z)
            removeIndex (_, y, z) = (y, z)

        rebuild :: (MonadTest m) => Vertex -> [(Unique, TypeSpecifier, ExprF Unique)] -> m (Expr GLfloat)
        rebuild root graph = do
          (ty, exprF) <- search root graph

          recursed <- traverse (fmap unExpr . flip rebuild graph) exprF
          pure $ Expr (ty :< recursed)

    output <- uncurry rebuild shared
    output === input
