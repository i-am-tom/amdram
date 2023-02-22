module Shader.Expression.SpecHook where

import Helper.Renderer (Renderer, withRenderer)
import Test.Hspec (Spec, SpecWith, around)

hook :: SpecWith Renderer -> Spec
hook = around withRenderer
