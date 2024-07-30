{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Interpolation.Interpolator as Interp
import Graphics.Primitives
import Graphics.ShaderLoader
import qualified Linear
import Data.Foldable (toList, foldrM)
import Control.Lens
import DataStructures.QuadTree
import qualified DataStructures.IOSpatialMap as SM
import qualified Data.Vector as V
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.UUID.V4
import qualified Inputs.MouseHandling as MouseHandling
import Graphics.Fonts
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Control.Monad (when, unless)
import GHC.Float (int2Float)
import Control.Monad.RWS.Strict (runRWST)

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error  !String
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
    { _envEventsChan      :: TQueue Event
    , _envWindow          :: !GLFW.Window
    , _envStateShaders    :: !(M.Map B.ByteString GL.Program)
    , _envTimeStart       :: !SystemTime
    , _envFreeTypeMapping :: !FreeTypeMapping
    }

data CpuGpuRepPair = CpuGpuRepPair {
      _cpuRep :: ObjectToRender
    , _gpuRep :: RenderedObject
}

instance Show CpuGpuRepPair where
    show CpuGpuRepPair{_cpuRep = a} = show a

instance PolygonShape CpuGpuRepPair where
  toPolygon o@CpuGpuRepPair{_cpuRep = a} = toPolygon a
  toBoundary o@CpuGpuRepPair{_cpuRep = a} = toBoundary a

$(makeLenses ''CpuGpuRepPair)


type OBS = IORef State

data State = State
    { _stateWindowWidth           :: !Int
    , _stateWindowHeight          :: !Int
    -- , _stateElementsOnScreen      :: !(SM.IOSpatialMap CpuGpuRepPair)
    , _stateRenderedCache         :: !(V.Vector RenderedObject)
    , _stateInteractable          :: !(M.Map String (MouseHandlingType, MouseHandling.MouseHandlingEvents IO MouseHandlingType))
    , _stateMousePos              :: !(Int, Int)
    , _stateMousePosPreviousFrame :: !(Int, Int)
    , _stateMouseButtonState      :: !MouseHandling.MouseButtonState
    , _stateFrameCounter          :: !Double
    , _stateCurrentSysTime        :: !SystemTime
                                    -- Time, function taking time difference, 
    , _statePlugins               :: !Plugins
    }

data Plugins = Plugins
    {
          _pluginsInterpolation:: !(Interp.InterpolationEnv, Interp.InterpolationState IO)
        , _pluginsMouseHandling:: !(MouseHandling.MouseHandlingEnv IO MouseHandlingType, MouseHandling.MouseHandlingState MouseHandlingType)
    }

type MouseHandlingType = CpuGpuRepPair

$(makeLenses ''State)
$(makeLenses ''Plugins)
$(makeLenses ''Interp.InterpolationEnv)
$(makeLenses ''Interp.InterpolationState)
$(makeLenses ''MouseHandling.MouseHandlingEnv)
$(makeLenses ''MouseHandling.MouseHandlingState)
$(makeLenses ''MouseHandling.MouseHandlingEvents)
$(makeLenses ''MouseHandling.ModifierKeys)


--------------------------------------------------------------------------------

-- getFromHashMap:: UUID -> IO (Maybe CpuGpuRepPair)
-- getFromHashMap uuid = do
--     interactable <- _stateInteractable <$> get

--     return $ M.lookup uuid interactable

insertToHashMap:: OBS -> (MouseHandling.Interactable MouseHandlingType, MouseHandling.MouseHandlingEvents IO MouseHandlingType) -> IO ()
insertToHashMap obs pair@(i, events) = do
    let handlingLens = pluginsMouseHandling . _1 . mouseHandlingEnvInteractables
    modifyIORef' obs $ \s -> s & (statePlugins . handlingLens) %~ \m -> M.insert (MouseHandling.interactableId i) pair m

    return()


createState:: Int -> Int -> IO State
createState width height = do

    -- spatialMap <- SM.createIOSpatialMap (20, 20) (width, height)
    sysTime <- getSystemTime

    plugins <- createPlugins

    return State
        { _stateWindowWidth           = width
        , _stateWindowHeight          = height
        -- , _stateElementsOnScreen      = spatialMap
        , _stateRenderedCache         = V.empty
        , _stateInteractable          = M.empty
        , _stateMousePos              = (0,0)
        , _stateMousePosPreviousFrame = (0,0)
        , _stateMouseButtonState      = MouseHandling.None
        , _stateFrameCounter          = 0
        , _stateCurrentSysTime        = sysTime
        , _statePlugins               = plugins
        }

createPlugins:: IO Plugins
createPlugins = do
    sysTime <- getSystemTime

    let interpEnv = Interp.InterpolationEnv {
            Interp._interpolationEnvTime = sysTime
        }

    let interpState = Interp.InterpolationState { 
            Interp._interpolationStateInterpolators = []
        }

    let mouseEnv = MouseHandling.MouseHandlingEnv {
          MouseHandling._mouseHandlingEnvMouseCoordinates              = (0,0) 
        , MouseHandling._mouseHandlingEnvMouseCoordinatesPreviousFrame = (0,0)
        , MouseHandling._mouseHandlingEnvInteractables                 = M.empty
        , MouseHandling._mouseHandlingModifierKeys                     = MouseHandling.ModifierKeys False False False False 
        , MouseHandling._mouseHandlingMouseButtonState                 = MouseHandling.None
        , MouseHandling._mouseHandlingActivated                        = False
    }

    let mouseState = MouseHandling.MouseHandlingState {
          MouseHandling._mouseHandlingStateActiveInteractables = M.empty
        , MouseHandling._mouseHandlingPreviousMouseState       = MouseHandling.None
    }
    
    return Plugins {
          _pluginsInterpolation = (interpEnv, interpState)
        , _pluginsMouseHandling = (mouseEnv, mouseState)
    }


updatePluginInputs:: OBS -> IO ()
updatePluginInputs obs = do
    -- Interpolation plugin
    sysTime <- getSystemTime

    modifyIORef' obs $ \s -> s & (statePlugins . pluginsInterpolation . _1 . interpolationEnvTime) .~ sysTime

    -- Mouse plugin
    s <- readIORef obs

    -- This should may be differently handled
    -- mouseHandlingEvents <- MouseHandling.emptyEvent
    -- let ints = map 
    --         (\(id, interactable) -> (MouseHandling.Interactable (interactable ^. cpuRep) (show id), mouseHandlingEvents)) $ M.toList (s ^. stateInteractable)
    
    let previousMouseEnv = s ^. (statePlugins . pluginsMouseHandling . _1)
    let mouseEnv = previousMouseEnv {
          MouseHandling._mouseHandlingEnvMouseCoordinates              = s ^. stateMousePos
        , MouseHandling._mouseHandlingEnvMouseCoordinatesPreviousFrame = s ^. stateMousePosPreviousFrame
    }

    modifyIORef' obs $ \s -> s & (statePlugins . pluginsMouseHandling . _1 ) .~ mouseEnv 

    return ()


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
        sysTime <- getSystemTime
        mapping <- initFreeType
        let env = Env
              { _envEventsChan      = eventsChan
              , _envWindow          = win
              , _envStateShaders    = shaders
              , _envTimeStart       = sysTime
              , _envFreeTypeMapping = mapping
              }
        iorefState <- newIORef st
        runIO env iorefState

    putStrLn "ended!"

--------------------------------------------------------------------------------

processInnerStateMonad:: OBS -> Interp.InterpolationPlugin IO a -> IO a
processInnerStateMonad obs plugin = do 
    (env, previousState) <- ( _pluginsInterpolation . _statePlugins ) <$> readIORef obs
    (a, r, _) <- runRWST plugin env previousState 
    -- Monad within clicking can change environment variable
    (envNew, _) <- ( _pluginsInterpolation . _statePlugins ) <$> readIORef obs 
    modifyIORef' obs $ \s -> s & (statePlugins . pluginsInterpolation) .~ (envNew, r)
    return a


processInnerStateMonadMouse:: OBS -> MouseHandling.MouseHandlingPlugin IO MouseHandlingType b -> IO b
processInnerStateMonadMouse obs plugin = do 
    (env, previousState) <- ( _pluginsMouseHandling . _statePlugins ) <$> readIORef obs
    (a, r, _) <- runRWST plugin env previousState
    -- Monad within clicking can change environment variable
    (envNew, _) <- ( _pluginsMouseHandling . _statePlugins ) <$> readIORef obs
    modifyIORef' obs $ \s -> s & (statePlugins . pluginsMouseHandling) .~ (envNew, r)
    return a

startup:: Env -> OBS -> IO ()
startup env obs = do
    width  <- _stateWindowWidth <$> readIORef obs
    height <- _stateWindowHeight <$> readIORef obs


    let conf = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 0 1)}
    let thing = Circle{_circlePosition= Linear.V2 100 800, _diameter = 100 }

    program <- askForShader env "BasicShaders"

    cache <- bindToGL thing conf program

    let objectToRender = ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive thing
        ,   _objectToRenderConfig = conf
        ,   _objectToRenderIndexWithinBuffer = 1
        ,   _objectToRenderProgram = program
        ,   _objectToRenderID = "1"
    }

    let conf2 = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 1 1)}
    let thing2 = Circle{_circlePosition= Linear.V2 300 100, _diameter = 80 }

    cache2 <- bindToGL thing2 conf2 program

    let objectToRender2 = ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive thing2
        ,   _objectToRenderConfig = conf2
        ,   _objectToRenderIndexWithinBuffer = 1
        ,   _objectToRenderProgram = program
        ,   _objectToRenderID = "2"
    }

    let pair = CpuGpuRepPair objectToRender cache
    let pair2 = CpuGpuRepPair objectToRender2 cache2


    let recs = map (\(x, y) -> Rectangle (Linear.V2 x y) 10 10) [(x,y)| y <- [10,30..120],  x <- [10,30..120]]

    programInstanced <- askForShader env "InstancedShaders"

    cache3 <- bindInstancedToGL recs conf2 programInstanced

    pair3 <- imapM (\i rect -> do 
        uuid3 <- nextRandom
        return $ CpuGpuRepPair ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive rect
        ,   _objectToRenderConfig = conf2
        ,   _objectToRenderIndexWithinBuffer = i
        ,   _objectToRenderID = "a" ++ show i 
        ,   _objectToRenderProgram = programInstanced
    } cache3) recs

    let mapping = _envFreeTypeMapping env
    let chars = mapping "Hello world,\těščřžýáíéó" "Oswald-Regular.ttf"

    fontShader <- askForShader env "FontShaders"
    let t = Text (Linear.V2 200 200) chars
    cache4 <- bindToGL t conf2 fontShader 


    let chars2 = mapping (show [x | x <- [1..12]]) "ComicNeue-Regular.ttf"

    let t = Text (Linear.V2 200 300) chars2 
    cache5 <- bindToGL t conf2 fontShader 
    -- isCancelledRef <- liftIO $ newIORef False

    mouseHandlingEvents <- MouseHandling.emptyEvent

    let someOnClick (x,y) modifierKeys (element, elementEvents) id = do
            -- m_val <- getFromHashMap uuid1
            -- m_valToFollow <- getFromHashMap uuid2
            -- let m_obj = (_objectToRenderPrimitiveObject . _cpuRep) <$> m_val 
            -- let m_objToFollow = (_objectToRenderPrimitiveObject . _cpuRep) <$> m_valToFollow
            -- let defaultOrAlreadyExisting = fromMaybe (toPrimitive thing) m_obj
            -- let defaultOrAlreadyExistingFollow = fromMaybe (toPrimitive thing2) m_objToFollow
            let floatX = int2Float x 
            let floatY = int2Float y

            -- let coordinatesToFollow = coords defaultOrAlreadyExistingFollow
            -- let startCoords = coords defaultOrAlreadyExisting
            processInnerStateMonad obs $ Interp.lerp2 1200 (Linear.V2 floatX floatY) (Linear.V2 (floatX + 100) (floatY + 100)) $ \vec -> do
                -- let conf = emptyConfig { _m_translate = Just $ (Linear.V3 a b 0)}
                let newV = case element ^. (cpuRep . objectToRenderPrimitiveObject) of
                        (PrimitiveCircle (Circle t d)) -> PrimitiveCircle $ Circle vec d 
                        _ -> newV
                let v = element & (cpuRep . objectToRenderPrimitiveObject) .~ newV 
                -- insertToHashMap $ (applyConfig element conf, mouseHandlingEvents & onMiceClicks .~ someOnClick)
                -- let iWithinBuffer = obj ^. objectToRenderIndexWithinBuffer
                -- insertToHashMap (MouseHandling.Interactable pair2 "2", mouseHandlingEvents & onMiceClicks .~ someOnClick)
                insertToHashMap obs (MouseHandling.Interactable v id, elementEvents)
                (v ^. (gpuRep . renderedObjectRebind)) (v ^. (cpuRep . objectToRenderPrimitiveObject)) 1 conf
                return ()
            return ()

                    --  & stateInteractable .~ M.fromList [pair, pair2] ++ pair3
    modifyIORef' obs $ \s -> s & stateRenderedCache .~ V.fromList [cache, cache2, cache3, cache4, cache5]

    insertToHashMap obs
        (
            MouseHandling.Interactable pair2 "2", 
            mouseHandlingEvents & onMiceClicks .~ someOnClick
                                & onMiceMove .~ onMouseMove obs
                                & onMiceEnter .~ onHover obs
                                & onMiceLeave .~ onHoverOut obs
        )
    -- insertToHashMap pair2
    -- mapM_ insertToHashMap pair3

    return ()


onHover :: OBS -> (a, b) -> p -> (CpuGpuRepPair, MouseHandling.MouseHandlingEvents IO MouseHandlingType) -> String -> IO ()
onHover obs (x,y) modKeys (element, elementEvents) id = do
    let conf = emptyConfig { 
        _m_backgroundColor = Just $ RGBA (Linear.V4 1 0 0 1)}

    let newV = case element ^. (cpuRep . objectToRenderPrimitiveObject) of
            (PrimitiveCircle (Circle t d)) -> PrimitiveCircle $ Circle t (200)
            _ -> newV

    let v = element & (cpuRep . objectToRenderPrimitiveObject) .~ newV
                    & (cpuRep . objectToRenderConfig) .~ conf
    insertToHashMap obs (MouseHandling.Interactable v id, elementEvents)
    (v ^. (gpuRep . renderedObjectRebind)) (v ^. (cpuRep . objectToRenderPrimitiveObject)) 1 conf
    return ()

onHoverOut :: OBS -> (a, b) -> p -> (CpuGpuRepPair, MouseHandling.MouseHandlingEvents IO MouseHandlingType) -> String -> IO ()
onHoverOut obs (x,y) modKeys (element, elementEvents) id = do
    let conf = emptyConfig { 
            _m_backgroundColor = Just $ RGBA (Linear.V4 0 1 0 1)}

    let newV = case element ^. (cpuRep . objectToRenderPrimitiveObject) of
            (PrimitiveCircle (Circle t d)) -> PrimitiveCircle $ Circle t (50)
            _ -> newV

    let v = element & (cpuRep . objectToRenderPrimitiveObject) .~ newV
                    & (cpuRep . objectToRenderConfig) .~ conf
    insertToHashMap obs (MouseHandling.Interactable v id, elementEvents)
    (v ^. (gpuRep . renderedObjectRebind)) (v ^. (cpuRep . objectToRenderPrimitiveObject)) 1 conf
    return ()

onMouseMove :: OBS -> (Int, Int) -> MouseHandling.ModifierKeys -> (CpuGpuRepPair, MouseHandling.MouseHandlingEvents IO MouseHandlingType) -> String -> IO ()
onMouseMove obs (x,y) modKeys (element, elementEvents) id = do
    when (MouseHandling._mouseHandlingCtrlPressed modKeys) $ do
        let newV = case element ^. (cpuRep . objectToRenderPrimitiveObject) of
                (PrimitiveCircle (Circle t d)) -> PrimitiveCircle $ Circle (Linear.V2 (int2Float x) (int2Float y)) (d)
                _ -> newV

        let v = element & (cpuRep . objectToRenderPrimitiveObject) .~ newV
                        -- & (cpuRep . objectToRenderConfig) .~ conf
        insertToHashMap obs (MouseHandling.Interactable v id, elementEvents)
        (v ^. (gpuRep . renderedObjectRebind)) (v ^. (cpuRep . objectToRenderPrimitiveObject)) 1 (v ^. (cpuRep . objectToRenderConfig))
    return ()

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


runIO :: Env -> OBS -> IO ()
runIO env obs = adjustWindow obs >> startup env obs >> run env obs 

askForShader:: Env -> B.ByteString -> IO GL.Program
askForShader env shaderName = do
    let programMap = _envStateShaders env

    return $ case programMap M.!? shaderName of
        Nothing -> error "Check shader name once again, is it created?"
        Just p -> p

run :: Env -> OBS -> IO ()
run env obs = do
    sysTime <- getSystemTime
    modifyIORef' obs $ \s -> s & stateFrameCounter %~ (+) 1
                     & stateCurrentSysTime .~ sysTime

    let win = _envWindow env

    cache <- _stateRenderedCache <$> readIORef obs

    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    mapM_ (draw obs) cache

    GLFW.swapBuffers win
    GL.flush  -- not necessary, but someone recommended it
    GLFW.pollEvents
    processEvents env obs

    processInnerStateMonad obs Interp.processInterpolations
    processInnerStateMonadMouse obs MouseHandling.processMouseEvents
    updatePluginInputs obs

    modifyIORef' obs $ \s -> s & stateMousePosPreviousFrame .~ (s ^. stateMousePos)

    q <- GLFW.windowShouldClose win
    unless q (run env obs)

processEvents :: Env -> OBS -> IO ()
processEvents env obs = do
    let tc = _envEventsChan env
    me <- atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent env obs e
          processEvents env obs
      Nothing -> return ()

printEvent :: String -> [String] -> IO ()
printEvent cbname fields =
    putStrLn $ cbname ++ ": " ++ unwords fields

setMousePos:: Env -> OBS -> Int -> Int -> IO ()
setMousePos env obs x y = do
    height <- _stateWindowHeight <$> readIORef obs
    modifyIORef' obs $ \s -> s & stateMousePos .~ (x, height - y)

activateMouseHandling:: OBS -> IO ()
activateMouseHandling obs = do
    modifyIORef' obs $ \s -> s & (statePlugins . pluginsMouseHandling . _1 . mouseHandlingActivated) .~ True

isMouseHandlingActivated:: OBS -> IO Bool
isMouseHandlingActivated obs =
    readIORef obs >>= \s -> return $ s ^. (statePlugins . pluginsMouseHandling . _1 . mouseHandlingActivated)
    
processEvent :: Env -> OBS -> Event -> IO ()
processEvent env obs ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          let win = _envWindow env
          GLFW.setWindowShouldClose win True

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modifyIORef' obs $ \s -> s
            { _stateWindowWidth  = width
            , _stateWindowHeight = height
            }
          adjustWindow obs

      (EventScroll _ x y) -> do
          adjustWindow obs

      (EventCursorPos _ x y) -> do
        -- screenHeight <- _stateWindowHeight <$> get
        -- grid <- _stateElementsOnScreen <$> get

        setMousePos env obs (fromEnum x) (fromEnum y)
        iMHA <- isMouseHandlingActivated obs
        -- Because both mouse positions were set at first frame at 0,0 it would cause
        -- Ray cast from top left corner to somewhere within the canvas
        unless iMHA $ 
            modifyIORef' obs $ \s -> s & stateMousePosPreviousFrame .~ (fromEnum x, fromEnum y)
        activateMouseHandling obs
        -- val <- lookupValueFromTree (floor x,screenHeight - floor y)

        return ()

      (EventKey win k scancode ks mk) -> do
          let mouseButtonLens = statePlugins . pluginsMouseHandling . _1 . mouseHandlingModifierKeys

          unless (ks == GLFW.KeyState'Repeating) $ case k of
            GLFW.Key'LeftControl  -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingCtrlPressed) .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'RightControl -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingCtrlPressed) .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'LeftAlt      -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingAltPressed)     .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'RightAlt     -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingAltPressed)     .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'LeftSuper    -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingSuperPressed)   .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'RightSuper   -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingSuperPressed)   .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'LeftShift    -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingShiftPressed)   .~ (ks == GLFW.KeyState'Pressed)
            GLFW.Key'RightShift   -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingShiftPressed)   .~ (ks == GLFW.KeyState'Pressed)
            _ -> return ()


          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Escape) $
                GLFW.setWindowShouldClose win True

      (EventMouseButton _ mb mba mk) -> do

        when (mb == GLFW.MouseButton'1) $ do
            let mouseButtonLens = statePlugins . pluginsMouseHandling . _1 
            case mba of
                GLFW.MouseButtonState'Pressed -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingMouseButtonState) .~ MouseHandling.PressDown
                GLFW.MouseButtonState'Released -> modifyIORef' obs $ \s -> s & (mouseButtonLens . mouseHandlingMouseButtonState) .~ MouseHandling.Released

            return ()

      _ -> return ()

adjustWindow :: OBS -> IO ()
adjustWindow obs = do
    width  <- _stateWindowWidth <$> readIORef obs 
    height <- _stateWindowWidth <$> readIORef obs 
    let
        pos    = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
    return ()

setViewProjectionMatrix:: OBS -> GL.Program -> IO ()
setViewProjectionMatrix obs program = do
    (w,h) <- readIORef obs >>= \s -> return (_stateWindowWidth s, _stateWindowHeight s)

    let proj = Linear.ortho (0) ( (int2Float w)) 0 (int2Float h) (int2Float (-w)) (int2Float w)
    matProj <- (GL.newMatrix GL.RowMajor $ concatMap toList proj :: IO (GL.GLmatrix GL.GLfloat))

    projectionLocation <- GL.get (GL.uniformLocation program "projection")
    GL.uniform projectionLocation  GL.$= matProj

-- insertValueToQuadtree:: [CpuGpuRepPair] -> IO ()
-- insertValueToQuadtree elems = do
--     width <- _stateWindowWidth <$> get
--     height <- _stateWindowHeight <$> get
--     grid <- _stateElementsOnScreen <$> get
--     val <- liftIO $ foldrM (\spatialDat t ->
--         let x = spatialDat ^. cpuRep ^.  objectToRenderPrimitiveObject in
--         let (corner1, corner2)= toBoundary x in
--             SM.appendGridRange (corner1 & both %~ floor) (corner2 & both %~ floor) spatialDat t)
--         grid elems

--     return ()

-- lookupValueFromTree:: Coord -> IO [CpuGpuRepPair]
-- lookupValueFromTree coord@(px, py) = do
--     width <- _stateWindowWidth <$> get
--     height <- _stateWindowHeight <$> get

--     -- InstancedCache vals <- _stateRenderedCache <$> get
--     grid <- _stateElementsOnScreen <$> get
--     vals <- liftIO $ SM.lookupCoords coord grid
--     let numOfVals = length vals
    -- liftIO $ print grid
    -- val <- mapM (\a@SM.SpatialData{SM._spatialData = x} -> do
    --     let r = x ^. gpuRep ^. renderedObjectBuffers
    --     let obj = x ^. cpuRep
    --     let d = obj ^. objectToRenderPrimitiveObject
    --     let indexInBuffer = obj ^. objectToRenderIndexWithinBuffer
    --     let primitive = obj ^. objectToRenderPrimitiveObject
    --     when (isPointInsidePolygon d (int2Float px, int2Float py)) $ do
    --         let conf = emptyConfig {
    --             _m_backgroundColor = Just $ RGBA (Linear.V4 0 0 1 1), 
    --             _m_translate = Just $ Linear.V3 (toEnum px) (toEnum py) 0 }
    --         liftIO $ (x ^. gpuRep ^. renderedObjectBindConfig) conf 


    --     return x
    --     ) vals
    -- return val
    -- return []

draw:: OBS -> RenderedObject -> IO ()
draw obs renderedObject = do
    -- width <- _stateWindowWidth <$> readIORef obs 
    -- height <- _stateWindowHeight <$> readIORef obs 
    let vao = renderedObject ^. (renderedObjectBuffers . glBuffersObject)
    let program = renderedObject ^. renderedObjectProgram
    GL.currentProgram GL.$= Just program

    setViewProjectionMatrix obs program

    GL.bindVertexArrayObject GL.$= Just vao
    renderedObject ^. renderedObjectDrawCall
    GL.bindVertexArrayObject GL.$= Nothing

    return ()
