{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- A helper for testing fragment shaders.
--
-- This module provides 'withRenderer', which in turn provides 'Renderer': a
-- set of functions for testing fragment shaders.
--
-- Specifically, a 'Renderer' sets up an SDL window, and renders a triangle
-- into it with the given fragment shader. It then samples the colour at the
-- @(0, 0)@ pixel and returns it to the caller.
module Helper.Renderer
  ( Renderer
  , renderSource

  , withRenderer
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.String.Interpolate (__i)
import Foreign.ForeignPtr (mallocForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Graphics.Rendering.OpenGL (($=), GLfloat)
import Graphics.Rendering.OpenGL qualified as GL
import Linear (V2 (V2), V4)
import SDL qualified

-- | A 'Renderer' allows you to test a fragment shader. Given the source code
-- for a fragment shader, a 'Renderer' will return the colour generated by that
-- shader for the @(0, 0)@ pixel.
type Renderer :: Type
newtype Renderer = Renderer { renderSource :: ByteString -> IO (V4 GLfloat) }

-- | Create a 'Renderer', run the given function, and then destroy and cleanup
-- the 'Renderer' afterwards. We should probably do this with frame buffers
-- rather than making SDL windows, but I'm not clever enough to figure out how
-- to do that yet.
withRenderer :: (Renderer -> IO ()) -> IO ()
withRenderer k = do
  SDL.initialize [ SDL.InitVideo ]

  SDL.HintRenderDriver        $= SDL.OpenGL
  SDL.HintRenderOpenGLShaders $= SDL.EnableShaders
  SDL.HintRenderScaleQuality  $= SDL.ScaleLinear

  window <-
    SDL.createWindow mempty SDL.defaultWindow
      { SDL.windowGraphicsContext =
          SDL.OpenGLContext SDL.defaultOpenGL
            { SDL.glProfile = SDL.Core SDL.Debug 4 1
            }
      , SDL.windowVisible = False
      , SDL.windowInitialSize = V2 1 1
      }

  context <- SDL.glCreateContext window

  SDL.showWindow window
  SDL.glMakeCurrent window context

  canvas <- createCanvasVAO
  GL.bindVertexArrayObject $= Just canvas

  k Renderer
    { renderSource = \source -> do
        program <- createShaderProgram source

        GL.currentProgram $= Just program
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

        GL.clearColor $= GL.Color4 1 1 1 1
        GL.clear [ GL.ColorBuffer ]

        GL.drawArrays GL.Triangles 0 6

        mallocForeignPtr >>= \fptr ->
          withForeignPtr fptr \ptr -> do
            GL.readPixels (GL.Position 0 0) (GL.Size 1 1) do
              GL.PixelData GL.RGBA GL.Float ptr

            peek ptr
    }

  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit

-- | A triangle that contains the origin pixel. Guarantees that the square
-- given by @(0, 0)@ and @(1, 1)@ will be rendered by the fragment shader we
-- want to test.
createCanvasVAO :: IO GL.VertexArrayObject
createCanvasVAO = do
  vertexArrayObject <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vertexArrayObject

  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer

  withArray [ (-1), (-1), 3, (-1), (-1), 3 ] \(ptr :: Ptr GLfloat) ->
    GL.bufferData GL.ArrayBuffer $= ( 24, ptr, GL.StaticDraw )

  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)

  pure vertexArrayObject

-- | Create a shader program using the given fragment shader source. This
-- fragment shader will be applied to a single triangle, and the pixel at
-- @(0, 0)@  will be sampled for testing.
createShaderProgram :: ByteString -> IO GL.Program
createShaderProgram fragmentShaderSource = do
  program <- GL.createProgram

  vertexShader <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vertexShader $= [__i|
    \#version 410 core

    layout(location = 0) in vec2 position;

    void main(void) {
      gl_Position = vec4(position, 0, 1);
    }
  |]

  GL.compileShader vertexShader
  GL.attachShader program vertexShader

  fragmentShader <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fragmentShader $= fragmentShaderSource

  GL.compileShader fragmentShader

  GL.compileStatus fragmentShader >>= flip unless do
    GL.get (GL.shaderInfoLog fragmentShader) >>= fail

  GL.attachShader program fragmentShader

  GL.linkProgram program
  GL.get (GL.linkStatus program) >>= flip unless do
    GL.get (GL.programInfoLog program) >>= fail

  GL.validateProgram program
  GL.get (GL.validateStatus program) >>= flip unless do
    GL.get (GL.programInfoLog program) >>= fail

  pure program
