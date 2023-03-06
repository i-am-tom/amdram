{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.ShareSpec where

import Control.Applicative (liftA3)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Graph (Vertex)
import Data.Some (Some)
import Data.Some qualified as Some
import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog (Gen, MonadTest, annotateShow, evalIO, failure, footnoteShow, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderAssignments, renderExpr)
import Helper.Roughly (isRoughly)
import Language.GLSL.Syntax (TypeSpecifier)
import Linear (V3)
import Shader.Expression (Assignments (..), lift, share, vec2, vec3, vec4)
import Shader.Expression.Core (Expr (Expr, unExpr), ExprF (FunctionCall, Variable), unsafeLift)
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

    let unwrap :: Some Expr -> Cofree ExprF TypeSpecifier
        unwrap = Some.foldSome unExpr

    Assignments {root, graph} <- liftIO (share input)
    annotateShow (root, map (second unwrap) graph)

    let search :: (MonadTest m) => Vertex -> m (Cofree ExprF TypeSpecifier)
        search index = case lookup index graph of
          Nothing -> footnoteShow index *> failure
          Just expr -> pure (unwrap expr)

        resolve :: (MonadTest m) => Cofree ExprF TypeSpecifier -> m (Cofree ExprF TypeSpecifier)
        resolve (ty :< expression) = case expression of
          Variable n -> search n >>= resolve
          nonterminal -> fmap (ty :<) (traverse resolve nonterminal)

    output <- search root >>= resolve
    Expr output === input

  it "sharing yields the same results" \renderer -> hedgehog do
    Expr expr <- forAll $ liftA3 vec3 genShader genShader genShader

    let input :: Expr (V3 GLfloat)
        input = unsafeLift $ FunctionCall "normalize" [expr]

    shared <- evalIO (share input >>= renderAssignments renderer)
    unshared <- evalIO (renderExpr renderer input)

    shared `isRoughly` unshared
