{-# LANGUAGE BlockArguments #-}

-- |
-- A utility for observing potential intermediate results in a GLSL shader.
module Shader.Expression.Share where

import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Foldable (toList)
import Data.Graph (Vertex, graphFromEdges', reverseTopSort)
import Data.Reify (Unique, reifyGraph)
import Data.Reify qualified as Reify
import Language.GLSL.Syntax qualified as Syntax
import Shader.Expression.Core (Expr (Expr), ExprF (..))

-- | Consider the following shader:
--
--     let v = vec2 1 2 in v.x + v.y
--
-- In our basic DSL, @let@ bindings are shallow. What this means is that the
-- above shader is actually identical to
--
--     (vec2 1 2).x + (vec2 1 2).y
--
-- We can see how more complex shaders will end up with a /lot/ of duplication,
-- especially if they involve accessing multiple parts of an intermediate value
-- like our @v@ here.
--
-- What we'd like to do is try to detect these points at which a value (@v@ in
-- our case) is being referenced in more than one node of our AST. We can then
-- abstract these points out to intermediate variables, and hopefully improve
-- our performance.
--
-- The downside is that we can no longer talk about our expression language as
-- being one-to-one with expressions in GLSL: we now have to distinguish
-- between the final assignments' expressions and the variables that must be
-- bound before that.
--
-- Worse still, these variable assignments have a significant order: you can't
-- reference a variable before it has been bound. This means we need to keep
-- track of the /ordered/ list of assignments required, plus the final
-- expression and its relationship to those bindings.
--
-- To do this, we use Andy Gill's type-safe observable sharing ('Data.Reify')
-- to generate a dependency graph. We then perform a reverse topological sort
-- on that graph to get our ordered list of assignments. Finally, the "root" of
-- the graph (as described by 'reifyGraph') gives us the variable to use for
-- the final assignment.
--
-- There's a bunch of stuff I'd love to make cleverer here. For one, there's
-- plenty we could do with common subexpression elimination. We could also be a
-- bit cleverer about /what/ we bind as an intermediate: is it worth having an
-- assignment like @int v0 = 1@ rather than just using @1@ directly? In any
-- case, these are problems to solve when they become problems.
share :: Expr x -> IO (Vertex, [(Unique, Syntax.TypeSpecifier, ExprF Unique)])
share (Expr cofree) = do
  Reify.Graph mappings entry <- reifyGraph cofree

  let -- Generate a directed, acyclic graph to represent the value dependencies
      -- within our program. @containers@ is a gift of a library.
      (graph, nodeFromVertex) =
        graphFromEdges'
          [ (node, key, toList node)
            | (key, node) <- mappings
          ]

      -- A reverse topological sort should organise the assignments such that an
      -- assignment only occurs /after/ its dependents have been assigned.
      matches :: [(Unique, Syntax.TypeSpecifier, ExprF Unique)]
      matches = do
        (typeSpecifier :< expression, variable, _) <-
          map nodeFromVertex (reverseTopSort graph)

        pure (variable, typeSpecifier, expression)

  pure (entry, matches)
