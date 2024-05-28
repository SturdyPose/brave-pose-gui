{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO, modify)
-- import Text.PrettyPrint   hiding ((<>))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import Graphics.Primitives
import Graphics.ShaderLoader
import qualified Linear
import Data.Foldable (toList, foldrM)
import GHC.Float (int2Float)
import Data.IORef
import Control.Lens
import DataStructures.QuadTree
import qualified DataStructures.IOSpatialMap as SM
import Foreign (with)
import Control.Monad (forM_)
import Foreign.Marshal (withArray)
import Data.Maybe (fromMaybe)


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
    }

data RenderObjects = RenderObjects {
      _spatialDataObjectToRender:: ObjectToRender
    , _spatialDataRenderedObject:: RenderedObject
} 

instance Show RenderObjects where
    show RenderObjects{_spatialDataObjectToRender = a} = show a

$(makeLenses ''RenderObjects)

data State = State
    { _stateWindowWidth      :: !Int
    , _stateWindowHeight     :: !Int
    , _stateDidOnce          :: IORef Bool
    , _stateElementsOnScreen :: SM.IOSpatialMap RenderObjects 
    , _stateCache            :: [RenderedObject]
    }

$(makeLenses ''State)


type Demo = RWST Env () State IO

--------------------------------------------------------------------------------

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
        didOnceRef <- newIORef False
        spatialMap <- SM.createIOSpatialMap (20, 20) (fbWidth, fbHeight)
        let env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
              }
            state = State
              { _stateWindowWidth     = fbWidth
              , _stateWindowHeight    = fbHeight
              , _stateDidOnce         = didOnceRef
              , _stateElementsOnScreen = spatialMap
              , _stateCache            = []
              }
        runDemo env state

    putStrLn "ended!"

--------------------------------------------------------------------------------

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
    let shaders = [
                ShaderInfo GL.VertexShader   (FileSource "shaders/vertex.shader"),
                ShaderInfo GL.FragmentShader (FileSource "shaders/fragment.shader")
            ]
    program <- loadShaders shaders
    void $ evalRWST (adjustWindow >> run program) env state

run :: GL.Program -> Demo ()
run program = do
    win <- asks envWindow

    draw program
    liftIO $ do
        GLFW.swapBuffers win
        GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents

    q <- liftIO $ GLFW.windowShouldClose win
    unless q (run program)

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

        -- v <- liftIO $ SM.forEach grid (\column row x -> do 
        --     print (column, row))
            -- if odd row then 
            --     return $ drawBasedOnConfig emptyConfig {_m_color = Just $ RGBA $ Linear.V4 0 1.0 0 0.5} (SM._spatialData x) 
            -- else 
            --     return (return ():: Demo ())) 
        -- traverse v
        -- sequence_ v
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

once:: Demo a -> Demo (Maybe a)
once callback = do
    didOnce <- _stateDidOnce <$> get
    didOnceVal <- liftIO $ readIORef didOnce
    val <- if didOnceVal then return Nothing else Just <$> callback
    liftIO $ writeIORef didOnce True
    return val

insertValueToQuadtree:: [RenderObjects] -> Demo ()
insertValueToQuadtree elems = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    grid <- _stateElementsOnScreen <$> get
    val <- liftIO $ foldrM (\spatialDat t ->
        let x = spatialDat ^. spatialDataObjectToRender ^.  objectToRenderPrimitiveObject in
        let (corner1, corner2)= toBoundary x in
            SM.appendGridRange (corner1 & both %~ floor) (corner2 & both %~ floor) spatialDat t)
        grid elems


    return ()

lookupValueFromTree:: Coord -> Demo [RenderObjects]
lookupValueFromTree coord@(px, py) = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    -- InstancedCache vals <- _stateCache <$> get
    grid <- _stateElementsOnScreen <$> get
    vals <- liftIO $ SM.lookupCoords coord grid
    let numOfVals = length vals
    -- liftIO $ print grid
    val <- mapM (\a@SM.SpatialData{SM._spatialData = x} -> do
        let r = x ^. spatialDataRenderedObject ^. renderedObjectBuffers 
        let obj = x ^. spatialDataObjectToRender
        let d = obj ^. objectToRenderPrimitiveObject
        let indexInBuffer = obj ^. objectToRenderIndexWithinBuffer
        let primitive = obj ^. objectToRenderPrimitiveObject
        -- let 
        when (isPointInsidePolygon d (int2Float px, int2Float py)) $ do
            if odd indexInBuffer then 
                drawBasedOnConfig emptyConfig {_m_color = Just $ RGBA $ Linear.V4 1.0 0 0 1.0} x
            else
                drawBasedOnConfig emptyConfig {_m_color = Just $ RGBA $ Linear.V4 0 1.0 0 0.5} x

        return x
        ) vals
    
    return val

draw :: GL.Program -> Demo ()
draw program = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    liftIO $ do
        GL.currentProgram GL.$= Just program
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- let pix = Pixel{_position = Linear.V2 (0.0) 0 } 
    setViewProjectionMatrix program
    let conf = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 0 1)}
    let conf2 = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 0.5 1 1)}
    -- let things = [if float2Int (x + y) `mod` 2 == 0 then (Circle{_position = Linear.V2 (20 * x + 5) (20 * y + 5 ) , _diameter= 10 }, conf) else (Rectangle {_position = Linear.V2 (20 * x) (20 * y) , _width= 10, _height = 10 }, conf) | x<-[1..100], y<-[1..100]]

    -- let things = [Circle{_circlePosition = Linear.V2 (x * 10) (20*sin x + 100), _diameter = 10 }| x <- [0..20]] 
    -- let things = [] :: Circle
    let things2 = [Circle{_circlePosition= Linear.V2 (x * 45) (y * 45) , _diameter = 40 }| x <- [0..(int2Float width) / 45], y <- [0..(int2Float height) / 45]] 

    let objectToRender a = fst $ foldl (\(acc, i) x-> ((ObjectToRender (toPrimitive x) conf i):acc, i+1)) ([], 0) a

    m_instancedRenderingCache <- once $ do
        -- let r_circ = RenderablePrimitive {_primitives = [(circ, conf), (circ2, conf)], _m_descriptor = Nothing }
        -- cache <- liftIO $ bindToGL things conf program 
        cache2 <- liftIO $ bindToGL things2 conf2 program 

        -- let dat = map (\x -> RenderObjects {_spatialDataObjectToRender = x, _spatialDataRenderedObject = cache }) $ objectToRender things 
        let dat2 = map (\x -> RenderObjects {_spatialDataObjectToRender = x, _spatialDataRenderedObject = cache2 }) $ objectToRender things2

        -- insertValueToQuadtree (dat ++ dat2)
        insertValueToQuadtree dat2

        modify $ \s -> s & stateCache .~ [cache2]
        -- modify $ \s -> s & stateCache .~ [cache,cache2]
        return ()

    -- _ <- liftIO $ render program r_circ
    case m_instancedRenderingCache of
        Just _ -> return ()
        Nothing -> do
            cache <- (_stateCache) <$> get
            forM_ cache $ \c -> liftIO $ instancedRenderingFromCache program c
            return ()

    return ()


whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

drawBasedOnConfig:: Config -> RenderObjects -> Demo ()
drawBasedOnConfig conf renderObjects = do
    let objToRender = renderObjects  ^. spatialDataObjectToRender
    let objIndex = objToRender ^. objectToRenderIndexWithinBuffer
    let prim = objToRender ^. objectToRenderPrimitiveObject
    let glBuffers = renderObjects ^. spatialDataRenderedObject ^. renderedObjectBuffers

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