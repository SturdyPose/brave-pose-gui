{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.Primitives where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear
import Control.Lens (makeLenses, (^.), (%~), (&), (.~))
import Foreign (withArray, plusPtr, nullPtr, sizeOf, Bits (xor))
import Foreign.Ptr (Ptr)
import GHC.Float (int2Float)
import Data.Foldable (foldrM, Foldable (toList))
import qualified Graphics.GL as GLRaw
import qualified Data.Vector as V
import Control.Monad (forM_)
import Data.Maybe (listToMaybe, isJust, fromMaybe, fromJust)
import Foreign.Marshal (with)
import GHC.Real (infinity)
import qualified Data.Vector.Storable as VS
import Foreign.Storable (peek)
import Data.IORef (newIORef, writeIORef, modifyIORef)
import GHC.IORef (readIORef)
import GHC.Generics (U1 (..), type (:*:) (..), K1 (..), M1 (..), Generic (..))
import Control.Applicative (Alternative ((<|>)))
import Data.UUID

floatSize64  = (fromIntegral $ sizeOf (0.0::GL.GLfloat)) :: GL.GLsizeiptr
floatSize  = (fromIntegral $ sizeOf (0.0::GL.GLfloat)) :: GL.GLsizei
v2Size = 2 * floatSize
v4Size = 2 * v2Size

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

degreesToRad:: Floating a => a -> a
degreesToRad a = a / 180 * pi

data ColorType =
      RGB (Linear.V3 Float)
    | RGBA (Linear.V4 Float)
    | HEX String
    | LinearGradient ColorType ColorType
    deriving (Show, Eq, Ord)

colorTypeToRGBA:: ColorType -> Linear.V4 GL.GLfloat
colorTypeToRGBA (RGB (Linear.V3 x y z)) = Linear.V4 x y z 100
colorTypeToRGBA (RGBA a) = a
colorTypeToRGBA _ = error "Not implemented"


$(makeLenses ''ColorType)

data Events = MouseHoverIn | MouseHoverOut
  deriving Enum


data Config = Config {
      _m_backgroundColor :: Maybe ColorType
    , _m_color           :: Maybe ColorType
    , _m_dimensions      :: Maybe (Linear.V4 Float)
    , _m_scale           :: Maybe (Linear.V3 Float)
    , _m_translate       :: Maybe (Linear.V3 Float)
} deriving (Show, Eq, Ord, Generic)

emptyConfig:: Config
emptyConfig = Config {
    _m_backgroundColor = Nothing
  , _m_color           = Nothing
  , _m_dimensions      = Nothing
  , _m_scale           = Nothing
  , _m_translate       = Nothing
}

class GCombine f where
  gCombine :: f p -> f p -> f p

instance GCombine U1 where
  gCombine U1 U1 = U1

instance (GCombine a, GCombine b) => GCombine (a :*: b) where
  gCombine (a1 :*: b1) (a2 :*: b2) = gCombine a1 a2 :*: gCombine b1 b2

instance (Alternative f) => GCombine (K1 i (f a)) where
  gCombine (K1 a) (K1 b) = K1 (a <|> b)

instance (GCombine a) => GCombine (M1 i c a) where
  gCombine (M1 a) (M1 b) = M1 (gCombine a b)

class Combine a where
  combine :: a -> a -> a
  default combine :: (Generic a, GCombine (Rep a)) => a -> a -> a
  combine x y = to $ gCombine (from x) (from y)

instance Combine Config

$(makeLenses ''Config)

newtype Polygon = Polygon { _points:: V.Vector(Linear.V2 GL.GLfloat) }
  deriving Show

data Pixel     = Pixel      { _pixelPosition:: !(Linear.V2 GL.GLfloat) }
  deriving Show

$(makeLenses ''Pixel)

data Line      = Line       { _linePosition:: !(Linear.V2 GL.GLfloat) }
  deriving Show

$(makeLenses ''Line)

data BezierLine= BezierLine { _bezierLinePosition:: !(Linear.V2 GL.GLfloat) }
  deriving Show

$(makeLenses ''BezierLine)

data Text      = Text       { _textPosition:: !(Linear.V2 GL.GLfloat) }
  deriving Show

$(makeLenses ''Text)

data Circle    = Circle     { _circlePosition:: !(Linear.V2 GL.GLfloat) , _diameter:: !GL.GLfloat }
  deriving Show

$(makeLenses ''Circle)

data Rectangle = Rectangle  { _rectanglePosition:: !(Linear.V2 GL.GLfloat) , _width:: !GL.GLfloat, _height:: !GL.GLfloat}
  deriving Show

$(makeLenses ''Rectangle)

data Ellipse   = Ellipse    { _ellipsePosition:: !(Linear.V2 GL.GLfloat) }
  deriving Show

$(makeLenses ''Ellipse)

data Triangle  = Triangle   { _trianglePosition:: !(Linear.V2 GL.GLfloat) }
  deriving Show

$(makeLenses ''Triangle)

data Primitive =
      PrimitivePixel       Pixel
    | PrimitiveLine        Line
    | PrimitiveBezierLine  BezierLine
    | PrimitiveText        Text
    | PrimitiveCircle      Circle
    | PrimitiveRectangle   Rectangle
    | PrimitiveEllipse     Ellipse
    | PrimitiveTriangle    Triangle
    | PrimitivePoly        Polygon
    deriving Show

$(makeLenses ''Primitive)

type Boundary = ((Float,Float), (Float,Float))

class PrimitiveShape a where
  toPrimitive:: a -> Primitive

class PolygonShape a where 
  toPolygon:: a -> Polygon 
  toBoundary:: a -> Boundary
  applyConfig:: a -> Config -> a

instance PolygonShape Primitive where
  toPolygon (PrimitiveCircle      x) = toPolygon x
  toPolygon (PrimitiveRectangle   x) = toPolygon x
  toPolygon (PrimitivePixel       x) = error "Not implemented"
  toPolygon (PrimitiveLine        x) = error "Not implemented"
  toPolygon (PrimitiveBezierLine  x) = error "Not implemented"
  toPolygon (PrimitiveText        x) = error "Not implemented"
  toPolygon (PrimitiveEllipse     x) = error "Not implemented"
  toPolygon (PrimitiveTriangle    x) = error "Not implemented"
  toPolygon (PrimitivePoly        x) = toPolygon x

  toBoundary (PrimitiveCircle      x) = toBoundary x
  toBoundary (PrimitiveRectangle   x) = toBoundary x
  toBoundary (PrimitivePixel       x) = error "Not implemented"
  toBoundary (PrimitiveLine        x) = error "Not implemented"
  toBoundary (PrimitiveBezierLine  x) = error "Not implemented"
  toBoundary (PrimitiveText        x) = error "Not implemented"
  toBoundary (PrimitiveEllipse     x) = error "Not implemented"
  toBoundary (PrimitiveTriangle    x) = error "Not implemented"
  toBoundary (PrimitivePoly        x) = toBoundary x

  applyConfig (PrimitiveCircle      x) conf = PrimitiveCircle $ applyConfig x conf
  applyConfig (PrimitiveRectangle   x) conf = PrimitiveRectangle $ applyConfig x conf 
  applyConfig (PrimitivePixel       x) conf = error "Not implemented"
  applyConfig (PrimitiveLine        x) conf = error "Not implemented"
  applyConfig (PrimitiveBezierLine  x) conf = error "Not implemented"
  applyConfig (PrimitiveText        x) conf = error "Not implemented"
  applyConfig (PrimitiveEllipse     x) conf = error "Not implemented"
  applyConfig (PrimitiveTriangle    x) conf = error "Not implemented"
  applyConfig (PrimitivePoly        x) conf = error "Not implemented"

instance PolygonShape Polygon where
  -- CCW polygons, always check if polygons are ccw
  toPolygon x = x
  toBoundary _ = error "Not implemented"

instance PolygonShape Circle where
  toPolygon (Circle {_circlePosition = Linear.V2 x y, _diameter = d}) = Polygon { _points = genCirclePoints (x,y) d }
  toBoundary Circle{_circlePosition = (Linear.V2 x y), _diameter = d} = ((x - d/2, y - d/2), (x + d/2, y + d/2))

  applyConfig circ@Circle{_circlePosition = pos, _diameter = d} conf = 
    case conf of 
      Config { _m_translate = Just trans, _m_scale = Just v } -> Circle {_circlePosition = trans ^. Linear._xy, _diameter = v  ^. Linear._x }
      Config { _m_scale = Just v } -> circ{_diameter = v  ^. Linear._x }
      Config { _m_translate = Just trans } -> circ{_circlePosition = trans ^. Linear._xy}
      c -> circ


instance PolygonShape Rectangle where
  toPolygon (Rectangle  {_rectanglePosition = Linear.V2 x y, _width = w, _height = h }) = Polygon 
    { _points= V.fromList [Linear.V2 x y, Linear.V2 (x + w) y, Linear.V2 (x + w) (y + h), Linear.V2 x (y + h) ]}
  toBoundary Rectangle{_rectanglePosition = (Linear.V2 x y), _width = w, _height = h} = ((x, y), (x + w, y + h))

  applyConfig rect conf = rect
    -- case conf of 
    --   Config { _m_translate = Just trans, _m_scale = Just v } -> Circle {_circlePosition = trans ^. Linear._xy, _diameter = v  ^. Linear._x }
    --   Config { _m_scale = Just v } -> circ{_diameter = v  ^. Linear._x }
    --   Config { _m_translate = Just trans } -> circ{_circlePosition = trans ^. Linear._xy}
    --   c -> circ

  -- toPolygon _             = error "Not implemented"


initPixel      = Pixel     { _pixelPosition= Linear.V2 0 0 }
initLine       = Line      { _linePosition= Linear.V2 0 0 }
initBezierLine = BezierLine{ _bezierLinePosition= Linear.V2 0 0 }
initText       = Text      { _textPosition= Linear.V2 0 0 }
initCircle     = Circle    { _circlePosition= Linear.V2 0 0 , _diameter = 1 }
initRectangle  = Rectangle { _rectanglePosition= Linear.V2 0 0 , _width = 1, _height = 1}
initEllipse    = Ellipse   { _ellipsePosition= Linear.V2 0 0 }
initTriangle   = Triangle  { _trianglePosition= Linear.V2 0 0 }
initPolygon    = Polygon   { _points = V.empty }


numOfCirclePoints = 36
numOfCirclePointsInGL = numOfCirclePoints + 4 

genCirclePoints:: (Float, Float) -> Float -> V.Vector (Linear.V2 Float)
genCirclePoints (x,y) diameter = segmentParts
    where 
      startAngle = 0
      endAngle = 360
      minAngle = min startAngle endAngle
      maxAngle = max startAngle endAngle
      stepLength = int2Float (maxAngle - minAngle) / (int2Float numOfCirclePoints)
      segmentList = V.fromList [0..numOfCirclePoints + 1]
      segmentParts = V.map (\segment ->
              let angle = int2Float segment * stepLength in
                Linear.V2 (cos (degreesToRad angle) * diameter/2 + x) (sin (degreesToRad angle) * diameter/2 + y)
            ) segmentList

genCirclePointsForGL:: (Float, Float) -> V.Vector (Linear.V2 Float)
genCirclePointsForGL (x,y) = Linear.V2 0.0 0.0 `V.cons` segmentParts
    where 
      startAngle = 0
      endAngle = 360
      minAngle = min startAngle endAngle
      maxAngle = max startAngle endAngle
      stepLength = int2Float (maxAngle - minAngle) / (int2Float numOfCirclePoints)
      segmentList = V.fromList [0..numOfCirclePoints + 1]
      segmentParts = V.map (\segment ->
              let angle = int2Float segment * stepLength in
                Linear.V2 (cos (degreesToRad angle) + x) (sin (degreesToRad angle) + y)
            ) segmentList

pointWithinBoundary:: (Float, Float) -> Boundary -> Bool
pointWithinBoundary (x,y) ((x1,y1), (x2,y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2


data GLBuffers = GlBuffers {
    _glBuffersObject          :: !GL.VertexArrayObject 
  , _glBuffersVertexBuffer    :: !GL.BufferObject
  , _glBuffersColorBuffer     :: !GL.BufferObject
  , _glBuffersTransformBuffer :: !GL.BufferObject
  , _glProgram                :: !GL.Program
} deriving Show
(makeLenses ''GLBuffers)

data RenderedObject = RenderedObject {
    _renderedObjectBuffers     :: !GLBuffers
  , _renderedObjectDrawCall    :: !(IO ())
  , _renderedObjectProgram     :: !GL.Program
  , _renderedObjectCount       :: !Int
  , _renderedObjectBindConfig  :: !(Int -> Config -> IO())
}
(makeLenses ''RenderedObject)

data ObjectToRender = ObjectToRender {
    _objectToRenderPrimitiveObject    :: !Primitive
  , _objectToRenderConfig             :: !Config
  , _objectToRenderIndexWithinBuffer  :: !Int
  , _objectToRenderUUID               :: !UUID
  , _objectToRenderProgram            :: !GL.Program
} deriving Show

(makeLenses ''ObjectToRender)


instancedRenderingFromCache:: GL.Program -> RenderedObject -> IO RenderedObject
instancedRenderingFromCache program obj@RenderedObject{} = do
  let vaoName = obj ^. renderedObjectBuffers ^. glBuffersObject
  GL.bindVertexArrayObject GL.$= Just vaoName
  obj ^. renderedObjectDrawCall

  GL.bindVertexArrayObject GL.$= Nothing
  return obj


scaleMat :: Num a => a -> a -> a -> Linear.V4 (Linear.V4 a)
scaleMat factorX factorY factorZ = Linear.scaled $ Linear.V4 factorX factorY factorZ 1

primitiveTransformMat:: Primitive -> Linear.M44 Float
primitiveTransformMat (PrimitiveCircle (Circle (Linear.V2 x y) dia)) = Linear.mkTransformation (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 x y 0) Linear.!*! (scaleMat (dia/2) (dia/2) (dia/2))
primitiveTransformMat (PrimitiveRectangle (Rectangle (Linear.V2 x y) width height)) = Linear.mkTransformation (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 x y 0) Linear.!*! (scaleMat width height 0)
primitiveTransformMat _ = error "Because of pattern match above this shouldn't happen, have you bucketed shapes?"

howToUpdate:: Primitive -> Config -> IO ()
howToUpdate _ conf = do
  return ()


-- Raycasting algorithm
isPointInsidePolygon:: PolygonShape a => a -> (Float, Float) -> Bool
isPointInsidePolygon a p@(x,y)
  | V.length points < 3 = error "Polygon must be made from at least 3 points"
  | pointWithinBoundary p boundary = odd (if rayIntersects lastPoint firstPoint then numOfCrosses + 1 else numOfCrosses)
  | otherwise = False
  where
   Polygon points = toPolygon a
   boundary = toBoundary a
   (firstPoint, pointsWithoutFirstOne) = fromJust $ V.uncons points
   lastPoint = V.last points
   (numOfCrosses , _ ) = V.foldr foldHelp (0, firstPoint) pointsWithoutFirstOne
   rayIntersects (Linear.V2 xi yi) (Linear.V2 xj yj) = let x_intersection = (xj - xi) * (y - yi) / (yj - yi) + xi in ((yi > y) /= (yj > y)) && (x <= x_intersection )
   foldHelp point (count, previousPoint) =
    if rayIntersects point previousPoint then (count+1, point) else (count, point)

class GLInstancedBindable a where
  bindInstancedToGL:: [a] -> Config -> GL.Program -> IO RenderedObject

instance GLInstancedBindable Circle where
  bindInstancedToGL :: [Circle] -> Config -> GL.Program -> IO RenderedObject
  bindInstancedToGL circles conf program = do
    vaoName <- GL.genObjectName
    [vertexBuffer, transformBuffer, colorBuffer] <- GL.genObjectNames 3
    GL.bindVertexArrayObject GL.$= Just vaoName
    let stride = 2 * floatSize
    let !numberOfElementsInBucket = length circles

    let colors = map (\conf -> maybe (Linear.V4 0 0 0 0) colorTypeToRGBA (conf ^. m_backgroundColor)) $ replicate numberOfElementsInBucket conf

    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer

    let segmentParts = [Linear.V2 0 0] ++ (V.toList $ genCirclePoints (0,0) 2)

    withArray segmentParts $ \ptr -> do
      let sizev = fromIntegral ((numOfCirclePoints*2 + 2) * sizeOf (0:: GL.GLfloat))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)

    let vPosition  = GL.AttribLocation 0
    GL.vertexAttribPointer vPosition GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float stride (bufferOffset (0:: GL.GLsizei)))
    GL.vertexAttribArray vPosition GL.$= GL.Enabled

    GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer GL.$= Just colorBuffer
    withArray colors $ \ptr -> do
      let sizev = fromIntegral ((length colors) * (fromEnum v4Size))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.DynamicDraw)

    GL.vertexAttribPointer (GL.AttribLocation 1) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float v4Size (bufferOffset 0))

    GLRaw.glVertexAttribDivisor 1 1

    let !transformMat = fmap (primitiveTransformMat . PrimitiveCircle) circles
    let !lengthOfTransformMat = length transformMat

    GL.bindBuffer GL.ArrayBuffer GL.$= Just transformBuffer
    withArray transformMat $ \ptr -> do
      let !sizev = fromIntegral (lengthOfTransformMat * 4 * (fromEnum v4Size))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)

    GL.vertexAttribArray (GL.AttribLocation 2) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 3) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 4) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 5) GL.$= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 2) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) (bufferOffset $ 0 * v4Size))

    GL.vertexAttribPointer (GL.AttribLocation 3) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) ((bufferOffset $ 1 * v4Size)))

    GL.vertexAttribPointer (GL.AttribLocation 4) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) ((bufferOffset $ 2 * v4Size)))

    GL.vertexAttribPointer (GL.AttribLocation 5) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) ((bufferOffset $ 3 * v4Size)))
    GLRaw.glVertexAttribDivisor 2 1
    GLRaw.glVertexAttribDivisor 3 1
    GLRaw.glVertexAttribDivisor 4 1
    GLRaw.glVertexAttribDivisor 5 1

    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.bindVertexArrayObject GL.$= Nothing
    let drawCall = GL.drawArraysInstanced GL.TriangleFan 0 (toEnum numOfCirclePoints) (toEnum numberOfElementsInBucket)

    let buffers = GlBuffers {
        _glBuffersObject           = vaoName
      , _glBuffersVertexBuffer     = vertexBuffer
      , _glBuffersColorBuffer      = colorBuffer
      , _glBuffersTransformBuffer  = transformBuffer
      , _glProgram                 = program
    }

    let renderedObject = RenderedObject {
        _renderedObjectBuffers  = buffers
      , _renderedObjectCount    = toEnum numberOfElementsInBucket
      , _renderedObjectDrawCall = drawCall
      , _renderedObjectProgram  = program
      , _renderedObjectBindConfig = \_ _ -> return ()
    }

    return renderedObject

instance GLInstancedBindable Rectangle where
  bindInstancedToGL :: [Rectangle] -> Config -> GL.Program -> IO RenderedObject
  bindInstancedToGL recs conf program = do
    vaoName <- GL.genObjectName
    [vertexBuffer, transformBuffer, colorBuffer] <- GL.genObjectNames 3
    GL.bindVertexArrayObject GL.$= Just vaoName
    let stride = 2 * floatSize
    let !numberOfElementsInBucket = length recs

    let colors = map (\conf -> maybe (Linear.V4 0 0 0 0) colorTypeToRGBA (conf ^. m_backgroundColor)) $ replicate numberOfElementsInBucket conf

    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    let segmentParts = [0,0, 0,1, 1,0,
                        0,1, 1,0, 1,1] :: [GL.GLfloat]

    withArray segmentParts $ \ptr -> do
      let sizev = fromIntegral (12 * sizeOf (0:: GL.GLfloat))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)

    let vPosition  = GL.AttribLocation 0
    GL.vertexAttribPointer vPosition GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float stride (bufferOffset (0:: GL.GLsizei)))
    GL.vertexAttribArray vPosition GL.$= GL.Enabled

    GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer GL.$= Just colorBuffer
    withArray colors $ \ptr -> do
      let sizev = fromIntegral ((length colors) * (fromEnum v4Size))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)

    GL.vertexAttribPointer (GL.AttribLocation 1) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float v4Size (bufferOffset 0))

    GLRaw.glVertexAttribDivisor 1 1

    let !transformMat = fmap (primitiveTransformMat . PrimitiveRectangle) recs
    let !lengthOfTransformMat = length transformMat

    GL.bindBuffer GL.ArrayBuffer GL.$= Just transformBuffer
    withArray transformMat $ \ptr -> do
      let !sizev = fromIntegral (lengthOfTransformMat * 4 * (fromEnum v4Size))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)


    GL.vertexAttribArray (GL.AttribLocation 2) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 3) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 4) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 5) GL.$= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 2) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) (bufferOffset $ 0 * v4Size))

    GL.vertexAttribPointer (GL.AttribLocation 3) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) ((bufferOffset $ 1 * v4Size)))

    GL.vertexAttribPointer (GL.AttribLocation 4) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) ((bufferOffset $ 2 * v4Size)))

    GL.vertexAttribPointer (GL.AttribLocation 5) GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float ((4 * v4Size )) ((bufferOffset $ 3 * v4Size)))
    GLRaw.glVertexAttribDivisor 2 1
    GLRaw.glVertexAttribDivisor 3 1
    GLRaw.glVertexAttribDivisor 4 1
    GLRaw.glVertexAttribDivisor 5 1

    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.bindVertexArrayObject GL.$= Nothing
    let buffers = GlBuffers {
        _glBuffersObject           = vaoName
      , _glBuffersVertexBuffer     = vertexBuffer
      , _glBuffersColorBuffer      = colorBuffer
      , _glBuffersTransformBuffer  = transformBuffer
      , _glProgram                 = program
    }

    let renderedObject = RenderedObject {
        _renderedObjectBuffers  = buffers
      , _renderedObjectCount    = toEnum numberOfElementsInBucket
      , _renderedObjectDrawCall = GL.drawArraysInstanced GL.Triangles 0 (toEnum 6) (toEnum numberOfElementsInBucket)
      , _renderedObjectProgram  = program
      , _renderedObjectBindConfig = \i newConf -> do 
        whenJust (_m_backgroundColor newConf) $ \col -> do

          GL.bindBuffer GL.ArrayBuffer GL.$= Just colorBuffer
          let Linear.V4 r g b a = colorTypeToRGBA col

          withArray [r,g,b,a] $ \pColor -> do
            let sizeOfV4 = 4 * (toEnum $ sizeOf r)
            GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (toEnum i * sizeOfV4) (sizeOfV4) pColor
            return ()

          GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
        
        return ()
    }

    return renderedObject


class GLBindable a where
  bindToGL:: a -> Config -> GL.Program -> IO RenderedObject

instance GLBindable Circle where
  bindToGL circ conf program = do 
    ioRefConfig <- newIORef conf
    vaoName <- GL.genObjectName
    [vertexBuffer, transformBuffer, colorBuffer] <- GL.genObjectNames 3
    GL.bindVertexArrayObject GL.$= Just vaoName
    let stride = 2 * floatSize

    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    let segmentParts = V.toList $ genCirclePointsForGL (0,0)

    withArray segmentParts $ \ptr -> do
      let sizev = fromIntegral ((numOfCirclePointsInGL * 2) * sizeOf (0:: GL.GLfloat))
      GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)

    let vPosition  = GL.AttribLocation 0
    GL.vertexAttribPointer vPosition GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float stride (bufferOffset (0:: GL.GLsizei)))
    GL.vertexAttribArray vPosition GL.$= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.bindVertexArrayObject GL.$= Nothing
    let buffers = GlBuffers {
        _glBuffersObject           = vaoName
      , _glBuffersVertexBuffer     = vertexBuffer
      , _glBuffersColorBuffer      = colorBuffer
      , _glBuffersTransformBuffer  = transformBuffer
      , _glProgram                 = program
    }

    let renderedObject = RenderedObject {
        _renderedObjectBuffers  = buffers
      , _renderedObjectCount    = 1
      , _renderedObjectDrawCall = do 

        config <- readIORef ioRefConfig
        let colors = maybe (Linear.V4 0 0 0 0) colorTypeToRGBA (config ^. m_backgroundColor)

        vcolorLoc <- GL.get (GL.uniformLocation program "vcolor")
        GL.uniform vcolorLoc GL.$= let Linear.V4 r g b a = colors in GL.Vector4 r g b a 

        let !transformMat = (primitiveTransformMat . PrimitiveCircle) circ

        let newTransform = maybe transformMat (\vec -> transformMat & Linear.translation .~ vec ) (config ^. m_translate) 

        matTrans <- GL.newMatrix GL.RowMajor $ concatMap toList newTransform:: IO (GL.GLmatrix GL.GLfloat)
        transformLoc <- GL.get (GL.uniformLocation program "transform")
        GL.uniform transformLoc  GL.$= matTrans

        GL.drawArrays GL.TriangleFan 0 $ toEnum numOfCirclePointsInGL
      , _renderedObjectProgram  = program
      , _renderedObjectBindConfig = \_ config -> do
          modifyIORef ioRefConfig $ \oldConf -> combine config oldConf
          return ()
    }

    return renderedObject

instance PrimitiveShape Circle where
  toPrimitive = PrimitiveCircle

instance PrimitiveShape Rectangle where
  toPrimitive = PrimitiveRectangle
