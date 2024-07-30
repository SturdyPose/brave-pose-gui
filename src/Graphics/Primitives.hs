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
import Foreign (withArray, plusPtr, nullPtr, sizeOf, Bits (xor, shiftL), alloca)
import Foreign.Ptr (Ptr)
import GHC.Float (int2Float)
import Data.Foldable (foldrM, Foldable (toList))
import qualified Graphics.GL as GLRaw
import qualified Data.Vector as V
import Control.Monad (forM_)
import Data.Maybe (listToMaybe, isJust, fromMaybe, fromJust, isNothing)
import Foreign.Marshal (with)
import GHC.Real (infinity)
import qualified Data.Vector.Storable as VS
import Foreign.Storable (peek)
import Data.IORef (newIORef, writeIORef, modifyIORef)
import GHC.IORef (readIORef)
import GHC.Generics (U1 (..), type (:*:) (..), K1 (..), M1 (..), Generic (..))
import Control.Applicative (Alternative ((<|>)))
import Data.UUID
import FreeType (FT_Library, FT_Face, ft_Set_Char_Size, ft_Load_Glyph, ft_Get_Char_Index, ft_Set_Pixel_Sizes)
import Graphics.Fonts 
import Data.Bits (shiftR, Bits (shift))

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
} deriving (Show, Eq, Ord, Generic)

emptyConfig:: Config
emptyConfig = Config {
    _m_backgroundColor = Nothing
  , _m_color           = Nothing
}

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

data Text      = Text       { _textPosition:: !(Linear.V2 GL.GLfloat), _textFreeTypeChars:: [FreeTypeCharacter]}
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

class Show a => PolygonShape a where
  toPolygon:: a -> Polygon
  toBoundary:: a -> Boundary

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


instance PolygonShape Polygon where
  -- CCW polygons, always check if polygons are ccw
  toPolygon x = x
  toBoundary _ = error "Not implemented"

instance PolygonShape Circle where
  toPolygon (Circle {_circlePosition = Linear.V2 x y, _diameter = d}) = Polygon { _points = genCirclePoints (x,y) d }
  toBoundary Circle{_circlePosition = (Linear.V2 x y), _diameter = d} = ((x - d/2, y - d/2), (x + d/2, y + d/2))


instance PolygonShape Rectangle where
  toPolygon (Rectangle  {_rectanglePosition = Linear.V2 x y, _width = w, _height = h }) = Polygon
    { _points= V.fromList [Linear.V2 x y, Linear.V2 (x + w) y, Linear.V2 (x + w) (y + h), Linear.V2 x (y + h) ]}
  toBoundary Rectangle{_rectanglePosition = (Linear.V2 x y), _width = w, _height = h} = ((x, y), (x + w, y + h))


initPixel      = Pixel     { _pixelPosition= Linear.V2 0 0 }
initLine       = Line      { _linePosition= Linear.V2 0 0 }
initBezierLine = BezierLine{ _bezierLinePosition= Linear.V2 0 0 }
initText ftLib ftFace = Text      { _textPosition= Linear.V2 0 0, _textFreeTypeChars = []}
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
  , _renderedObjectRebind      :: !(Primitive -> Int -> Config -> IO())
}
(makeLenses ''RenderedObject)

data ObjectToRender = ObjectToRender {
    _objectToRenderPrimitiveObject    :: !Primitive
  , _objectToRenderConfig             :: !Config
  , _objectToRenderIndexWithinBuffer  :: !Int
  , _objectToRenderID                 :: !String
  , _objectToRenderProgram            :: !GL.Program
} deriving Show

instance PolygonShape ObjectToRender where
  toPolygon ObjectToRender{_objectToRenderPrimitiveObject=p } = toPolygon p
  toBoundary ObjectToRender{_objectToRenderPrimitiveObject=p } = toBoundary p

(makeLenses ''ObjectToRender)

scaleMat :: Num a => a -> a -> a -> Linear.V4 (Linear.V4 a)
scaleMat factorX factorY factorZ = Linear.scaled $ Linear.V4 factorX factorY factorZ 1

primitiveTransformMat:: Primitive -> Linear.M44 Float
primitiveTransformMat (PrimitiveCircle (Circle (Linear.V2 x y) dia)) = Linear.mkTransformation (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 x y 0) Linear.!*! (scaleMat (dia/2) (dia/2) (dia/2))
primitiveTransformMat (PrimitiveRectangle (Rectangle (Linear.V2 x y) width height)) = Linear.mkTransformation (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 x y 0) Linear.!*! (scaleMat width height 0)
primitiveTransformMat (PrimitiveText (Text (Linear.V2 x y) _)) = Linear.mkTransformation (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 x y 0) Linear.!*! (scaleMat 0 0 0)
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

rayIntersectsPolygon:: PolygonShape a => a -> Linear.V2 Float -> Linear.V2 Float -> Bool
rayIntersectsPolygon p (Linear.V2 x0 y0) (Linear.V2 x1 y1)
  | numOfPolyPoints < 3 = error "Polygon must be made from at least 3 points"
  | isJust checkEachEdge = True 
  | otherwise = False
  where
    Polygon points = toPolygon p
    numOfPolyPoints = V.length points
    edgeIntersection (Linear.V2 x2 y2) (Linear.V2 x3 y3)
      | denominator == 0 = Nothing
      | t >= 0 && t <= 1 && u >= 0 && u <= 1 = Just t
      | otherwise = Nothing
      where
          denominator = (x0 - x1) * (y2 - y3) - (y0 - y1) * (x2 - x3)
          t = ((x0 - x2)*(y2 - y3) - (y0 - y2)*(x2 - x3))/ denominator
          u = -(((x0 - x1)*(y0 - y2) - (y0 - y1)*(x0 - x2)) / denominator)
    checkEachEdge = foldr (\i m_t -> if isNothing m_t then let firstPoint = points V.! i in let nextPoint = points V.! ((i + 1) `mod` numOfPolyPoints) in edgeIntersection firstPoint nextPoint else m_t) Nothing [0..numOfPolyPoints-1]

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
      , _renderedObjectRebind = \_ _ _ -> return ()
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
      , _renderedObjectRebind = \obj i newConf -> do
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
    ioRefCirc <- newIORef circ
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
        circle <- readIORef ioRefCirc 
        let colors = maybe (Linear.V4 0 0 0 0) colorTypeToRGBA (config ^. m_backgroundColor)

        vcolorLoc <- GL.get (GL.uniformLocation program "vcolor")
        GL.uniform vcolorLoc GL.$= let Linear.V4 r g b a = colors in GL.Vector4 r g b a

        let !transformMat = (primitiveTransformMat . PrimitiveCircle) circle

        -- let newTransform = maybe transformMat (\vec -> transformMat & Linear.translation .~ vec ) (config ^. m_translate)

        matTrans <- GL.newMatrix GL.RowMajor $ concatMap toList transformMat:: IO (GL.GLmatrix GL.GLfloat)
        transformLoc <- GL.get (GL.uniformLocation program "transform")
        GL.uniform transformLoc  GL.$= matTrans

        GL.drawArrays GL.TriangleFan 0 $ toEnum numOfCirclePointsInGL
      , _renderedObjectProgram  = program
      , _renderedObjectRebind = \(PrimitiveCircle cir) _ config -> do
          writeIORef ioRefConfig config 
          writeIORef ioRefCirc cir
          return ()
    }

    return renderedObject

instance PrimitiveShape Circle where
  toPrimitive = PrimitiveCircle

instance PrimitiveShape Rectangle where
  toPrimitive = PrimitiveRectangle


instance GLBindable Text where
  bindToGL :: Text -> Config -> GL.Program -> IO RenderedObject
  bindToGL t@Text{_textPosition = pos, _textFreeTypeChars = chars} conf program = do
    vaoName <- GL.genObjectName
    [vertexBuffer, transformBuffer, colorBuffer] <- GL.genObjectNames 3
    GL.bindVertexArrayObject GL.$= Just vaoName
    let stride = 0 * floatSize

    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer

    let vPosition  = GL.AttribLocation 0
    GL.vertexAttribPointer vPosition GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float stride (bufferOffset (0:: GL.GLsizei)))
    GL.vertexAttribArray vPosition GL.$= GL.Enabled
    let sizev = fromIntegral (24 * sizeOf (0:: GL.GLfloat))
    GL.bufferData GL.ArrayBuffer GL.$= (sizev, nullPtr, GL.DynamicDraw)

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
        GL.blend GL.$= GL.Enabled
        GL.frontFace GL.$= GL.CW
        GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

        let Linear.V2 x y = pos

        GL.bindVertexArrayObject GL.$= Just vaoName
        let colors = maybe (Linear.V4 0 0 0 0) colorTypeToRGBA (conf ^. m_backgroundColor)

        vcolorLoc <- GL.get (GL.uniformLocation program "vcolor")
        GL.uniform vcolorLoc GL.$= let Linear.V4 r g b a = colors in GL.Vector4 r g b a

        let !transformMat = (primitiveTransformMat . PrimitiveText) t

        -- let newTransform = maybe transformMat (\vec -> transformMat & Linear.translation .~ vec ) (conf ^. m_translate)

        matTrans <- GL.newMatrix GL.RowMajor $ concatMap toList transformMat:: IO (GL.GLmatrix GL.GLfloat)
        transformLoc <- GL.get (GL.uniformLocation program "transform")
        GL.uniform transformLoc  GL.$= matTrans

        -- TODO: Make this into fold, I just don't want to deal with it now
        uglyStartXRef <- newIORef x

        forM_ chars $ \character -> do
          let texId = _freeTypeCharacterTextureID character
          let Linear.V2 charSizeX charSizeY = _freeTypeCharacterSize character
          let Linear.V2 bearingX bearingY = _freeTypeCharacterBearing character
          let Linear.V2 advanceX advanceY = _freeTypeCharacterAdvance character

          nextX <- readIORef uglyStartXRef

          let scale = 1
          let xpos = nextX + int2Float bearingX * scale
          let ypos = y - int2Float (charSizeY - bearingY) * scale
          let w = int2Float charSizeX * scale
          let h = int2Float charSizeY * scale 

          let vertices = [
                    xpos    , ypos + h, 0, 0
                  , xpos    , ypos    , 0, 1
                  , xpos + w, ypos    , 1, 1

                  , xpos    , ypos + h, 0, 0
                  , xpos + w, ypos    , 1, 1
                  , xpos + w, ypos + h, 1, 0
                ]

          GL.textureBinding GL.Texture2D GL.$= Just texId
          GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
          withArray vertices $ \ptr -> do
            GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 (toEnum $ 24 * sizeOf (0:: GL.GLfloat)) ptr
          GL.bindBuffer GL.ArrayBuffer GL.$= Nothing

          GL.drawArrays GL.Triangles 0 6


          modifyIORef uglyStartXRef $ \val -> val + int2Float (advanceX `shiftR` 6) * scale

          return ()

        GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
        GL.textureBinding GL.Texture2D GL.$= Nothing
        GL.bindVertexArrayObject GL.$= Nothing
        
        GL.blend GL.$= GL.Disabled

      , _renderedObjectProgram  = program
      , _renderedObjectRebind = \(PrimitiveText t) _ config -> do
          -- modifyIORef ioRefConfig $ \oldConf -> combine config oldConf
          -- writeIORef ioRefCirc cir
          return ()
    }

    return renderedObject


coords:: Primitive -> Linear.V2 Float
coords (PrimitiveCircle      x) = _circlePosition x
coords (PrimitiveRectangle   x) = _rectanglePosition x
coords (PrimitivePixel       x) = error "Not implemented"
coords (PrimitiveLine        x) = error "Not implemented"
coords (PrimitiveBezierLine  x) = error "Not implemented"
coords (PrimitiveText        x) = error "Not implemented"
coords (PrimitiveEllipse     x) = error "Not implemented"
coords (PrimitiveTriangle    x) = error "Not implemented"
coords (PrimitivePoly        x) = error "Not implemented"