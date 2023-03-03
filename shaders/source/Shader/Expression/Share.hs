{-# LANGUAGE BlockArguments #-}

-- |
-- A utility for observing potential intermediate results in a GLSL shader.
module Shader.Expression.Share where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Foldable (toList)
import Data.Functor.Foldable (embed)
import Data.Graph (graphFromEdges', reverseTopSort)
import Data.Reify (Unique, reifyGraph)
import Data.Reify qualified as Reify
import Data.Some (Some (Some))
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
share :: Expr x -> IO (Unique, [(Unique, Some Expr)])
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
      ordered :: [(Unique, CofreeF ExprF Syntax.TypeSpecifier Unique)]
      ordered = do
        (typeSpecifier :< expression, variable, _) <-
          map nodeFromVertex (reverseTopSort graph)

        pure (variable, typeSpecifier :< expression)

      -- We need a type for variables, which means we need a way to resolve
      -- our variable references to figure out their type.
      resolve :: Unique -> Cofree ExprF Syntax.TypeSpecifier
      resolve identifier = case lookup identifier ordered of
        Just (type_ :< _) -> embed (type_ :< Variable identifier)
        Nothing -> error "Something is very wrong..."

      -- For each of our (ordered) assignments, we create an expression and a
      -- variable reference from which we cacn generate a variable name.
      assignments :: [(Unique, Some Expr)]
      assignments = do
        (variable, typeSpecifier :< expression) <- ordered

        let rebuilt :: Cofree ExprF Syntax.TypeSpecifier
            rebuilt = embed (typeSpecifier :< fmap resolve expression)

        pure (variable, Some (Expr rebuilt))

  pure (entry, assignments)
