{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO, modify)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Graphics.Primitives
import Graphics.ShaderLoader
import qualified Linear
import Data.Foldable (toList, foldrM)
import GHC.Float (int2Float)
import Control.Lens
import DataStructures.QuadTree
import qualified DataStructures.IOSpatialMap as SM
import Foreign (with)


--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , _envStateShaders:: !(M.Map B.ByteString GL.Program)
    }

data RawAndRenderedObjectPair = RawAndRenderedObjectPair {
      _rawFromPair      :: ObjectToRender
    , _renderedFromPair :: RenderedObject
}

instance Show RawAndRenderedObjectPair where
    show RawAndRenderedObjectPair{_rawFromPair = a} = show a

$(makeLenses ''RawAndRenderedObjectPair)

data State = State
    { _stateWindowWidth      :: !Int
    , _stateWindowHeight     :: !Int
    , _stateElementsOnScreen :: !(SM.IOSpatialMap RawAndRenderedObjectPair)
    , _stateRenderedCache    :: ![RenderedObject] -- This ain't linear memory, make this a vector
    }

$(makeLenses ''State)

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------


createState:: Int -> Int -> IO State
createState width height = do

    spatialMap <- liftIO $ SM.createIOSpatialMap (20, 20) (width, height)

    return State
        { _stateWindowWidth      = width
        , _stateWindowHeight     = height
        , _stateElementsOnScreen = spatialMap
        , _stateRenderedCache    = []
        }

main :: IO ()
main = do
    let width  = 800
        height = 800

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "GLFW-b-demo" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
        GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
        GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
        GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
        GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
        GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
        GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

        GLFW.swapInterval 1

        GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
        shaders <- compileShaders
        st <- createState fbWidth fbHeight
        let env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
              , _envStateShaders = shaders
              }
        runDemo env st

    putStrLn "ended!"

--------------------------------------------------------------------------------

startup:: Demo ()
startup = do
    state <- get
    let width  = _stateWindowWidth  state
        height = _stateWindowHeight state


    let conf = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 0 1)}
    let thing = Circle{_circlePosition= Linear.V2 100 200, _diameter = 100 }

    program <- askForShader "BasicShaders"

    cache3 <- liftIO $ bindToGL thing conf program

    let objectToRender = ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive thing
        ,   _objectToRenderConfig = conf
        ,   _objectToRenderIndexWithinBuffer = 1
        ,   _objectToRenderProgram = program
    }

    let pair = RawAndRenderedObjectPair objectToRender cache3

    modify $ \x -> x & stateRenderedCache .~ [cache3]
    insertValueToQuadtree [pair]

    return ()


-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = do
    void $ evalRWST (adjustWindow >> startup >> run) env state

askForShader:: B.ByteString -> Demo GL.Program
askForShader shaderName = do
    programMap <-  asks _envStateShaders

    return $ case programMap M.!? shaderName of
        Nothing -> error "Check shader name once again, is it created?"
        Just p -> p

run :: Demo ()
run = do
    win <- asks envWindow

    cache <- _stateRenderedCache <$> get
    mapM_ draw cache
    liftIO $ do
        GLFW.swapBuffers win
        GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents

    q <- liftIO $ GLFW.windowShouldClose win
    unless q run

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

processEvent :: Event -> Demo ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { _stateWindowWidth  = width
            , _stateWindowHeight = height
            }
          adjustWindow

      (EventScroll _ x y) -> do
          adjustWindow

      (EventCursorPos _ x y) -> do
        screenHeight <- _stateWindowHeight <$> get
        grid <- _stateElementsOnScreen <$> get

        val <- lookupValueFromTree (floor x,screenHeight - floor y)
        -- liftIO $ print val

        return ()

      (EventKey win k scancode ks mk) -> do
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True

      _ -> return ()

adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = _stateWindowWidth  state
        height = _stateWindowHeight state
        pos    = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
    return ()

setViewProjectionMatrix:: GL.Program -> Demo ()
setViewProjectionMatrix program = do
    (w,h) <- get >>= \s -> return (_stateWindowWidth s, _stateWindowHeight s)

    let proj = Linear.ortho (0) ( (int2Float w)) (0) (int2Float h) (int2Float (-w)) (int2Float w)
    matProj <- liftIO (GL.newMatrix GL.RowMajor $ concatMap toList proj :: IO (GL.GLmatrix GL.GLfloat))

    projectionLocation <- GL.get (GL.uniformLocation program "projection")
    GL.uniform projectionLocation  GL.$= matProj

insertValueToQuadtree:: [RawAndRenderedObjectPair] -> Demo ()
insertValueToQuadtree elems = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    grid <- _stateElementsOnScreen <$> get
    val <- liftIO $ foldrM (\spatialDat t ->
        let x = spatialDat ^. rawFromPair ^.  objectToRenderPrimitiveObject in
        let (corner1, corner2)= toBoundary x in
            SM.appendGridRange (corner1 & both %~ floor) (corner2 & both %~ floor) spatialDat t)
        grid elems

    return ()

lookupValueFromTree:: Coord -> Demo [RawAndRenderedObjectPair]
lookupValueFromTree coord@(px, py) = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    -- InstancedCache vals <- _stateRenderedCache <$> get
    grid <- _stateElementsOnScreen <$> get
    vals <- liftIO $ SM.lookupCoords coord grid
    let numOfVals = length vals
    -- liftIO $ print grid
    val <- mapM (\a@SM.SpatialData{SM._spatialData = x} -> do
        let r = x ^. renderedFromPair ^. renderedObjectBuffers
        let obj = x ^. rawFromPair
        let d = obj ^. objectToRenderPrimitiveObject
        let indexInBuffer = obj ^. objectToRenderIndexWithinBuffer
        let primitive = obj ^. objectToRenderPrimitiveObject
        when (isPointInsidePolygon d (int2Float px, int2Float py)) $ do
            liftIO $ print "Hello world"

        return x
        ) vals
    return val

draw:: RenderedObject -> Demo ()
draw renderedObject = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    let vao = renderedObject ^. (renderedObjectBuffers . glBuffersObject)
    let program = renderedObject ^. renderedObjectProgram
    liftIO $ do
        GL.currentProgram GL.$= Just program
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    setViewProjectionMatrix program

    liftIO $ do
        GL.bindVertexArrayObject GL.$= Just vao
        renderedObject ^. renderedObjectDrawCall
        GL.bindVertexArrayObject GL.$= Nothing

    return ()

--drawV2 :: ObjectToRender -> Demo ()
--drawV2 objectToRender = do
    --width <- _stateWindowWidth <$> get
    --height <- _stateWindowHeight <$> get
    --liftIO $ do
        --GL.currentProgram GL.$= Just program
        --GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    ---- let pix = Pixel{_position = Linear.V2 (0.0) 0 }
    --setViewProjectionMatrix program
    --let conf = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 0 1)}
    --let conf2 = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 0.5 1 1)}
    ---- let things = [if float2Int (x + y) `mod` 2 == 0 then (Circle{_position = Linear.V2 (20 * x + 5) (20 * y + 5 ) , _diameter= 10 }, conf) else (Rectangle {_position = Linear.V2 (20 * x) (20 * y) , _width= 10, _height = 10 }, conf) | x<-[1..100], y<-[1..100]]

    ---- let things = [Circle{_circlePosition = Linear.V2 (x * 10) (20*sin x + 100), _diameter = 10 }| x <- [0..20]] 
    ---- let things = [] :: Circle
    --let things2 = [Circle{_circlePosition= Linear.V2 (x * 15) (y * 15) , _diameter = 10 }| x <- [0..(int2Float width) / 15], y <- [0..(int2Float height) / 15]]

    --let objectToRender a = fst $ foldl (\(acc, i) x-> ((ObjectToRender (toPrimitive x) conf i):acc, i+1)) ([], 0) a

    --let thing = Circle{_circlePosition= Linear.V2 100 200, _diameter = 100 }

    --m_instancedRenderingCache <- once $ do
        ---- let r_circ = RenderablePrimitive {_primitives = [(circ, conf), (circ2, conf)], _m_descriptor = Nothing }
        ---- cache <- liftIO $ bindInstancedToGL things conf program
        --cache2 <- liftIO $ bindInstancedToGL things2 conf2 program
        --cache3 <- liftIO $ bindToGL thing conf program

        ---- let dat = map (\x -> RawAndRenderedObjectPair {_rawFromPair = x, _renderedFromPair = cache }) $ objectToRender things 
        --let dat2 = map (\x -> RawAndRenderedObjectPair {_rawFromPair = x, _renderedFromPair = cache2 }) $ objectToRender things2

        --let dat3 = RawAndRenderedObjectPair {_rawFromPair = ObjectToRender (toPrimitive thing) conf 0, _renderedFromPair = cache3}
        ---- insertValueToQuadtree (dat ++ dat2)
        ----insertValueToQuadtree (dat2 ++ [dat3])
        --insertValueToQuadtree ([dat3])

        --modify $ \s -> s & stateRenderedCache .~ [cache3]
        ---- modify $ \s -> s & stateRenderedCache .~ [cache,cache2]
        --return ()

    ---- _ <- liftIO $ render program r_circ
    --case m_instancedRenderingCache of
        --Just _ -> return ()
        --Nothing -> do
            --cache <- (_stateRenderedCache) <$> get
            --forM_ cache $ \c -> liftIO $ instancedRenderingFromCache program c
            --return ()

    --return ()

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

drawBasedOnConfig:: Config -> RawAndRenderedObjectPair -> Demo ()
drawBasedOnConfig conf rawAndRenderedObjectPair = do
    let objToRender = rawAndRenderedObjectPair  ^. rawFromPair
    let objIndex = objToRender ^. objectToRenderIndexWithinBuffer
    let prim = objToRender ^. objectToRenderPrimitiveObject
    let glBuffers = rawAndRenderedObjectPair ^. renderedFromPair ^. renderedObjectBuffers

    liftIO $ do
        whenJust (conf ^. m_color) (\color -> do
            GL.bindBuffer GL.ArrayBuffer GL.$= Just (glBuffers ^. glBuffersColorBuffer)
            with (colorTypeToRGBA color) $ \ptr -> do
                GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (toEnum $ objIndex * fromEnum v4Size) (floatSize64 * 4) ptr
                return ()
            return ())

        whenJust (conf ^. m_scale) (\scale -> do
            GL.bindBuffer GL.ArrayBuffer GL.$= Just (glBuffers ^. glBuffersTransformBuffer)
            let transformMat = primitiveTransformMat prim Linear.!*! (scaleMat scale scale scale)
            let lengthOfTransformMat = length transformMat
            with transformMat $ \ptr -> do
                let sizev = fromIntegral (lengthOfTransformMat * 4 * (fromEnum v4Size))
                GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer (toEnum $ objIndex * 16 * (fromEnum floatSize) ) (floatSize64 * 16) ptr
            return ())
        --   GL.bufferData GL.ArrayBuffer GL.$= (sizev, ptr, GL.StaticDraw)

        GL.bindBuffer GL.ArrayBuffer GL.$= Nothing

    -- Here update the object in the array
    return ()
