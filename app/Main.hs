{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void, forM)
import Control.Monad.RWS.Strict  (RWST, asks, evalRWST, get, liftIO, modify)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import Graphics.Primitives
import Graphics.ShaderLoader
import qualified Linear
import Data.Foldable (toList, foldrM)
import GHC.Float (int2Float, double2Float, castWord32ToFloat, castFloatToWord32)
import Control.Lens
import DataStructures.QuadTree
import qualified DataStructures.IOSpatialMap as SM
import qualified Data.Vector as V
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.UUID
import Data.UUID.V4
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, readIORef)
import GHC.IORef (writeIORef)

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
    { _stateWindowWidth           :: !Int
    , _stateWindowHeight          :: !Int
    , _stateElementsOnScreen      :: !(SM.IOSpatialMap RawAndRenderedObjectPair)
    , _stateRenderedCache         :: !(V.Vector RenderedObject)
    , _stateInteractable          :: !(HM.HashMap UUID RawAndRenderedObjectPair)
    , _stateMousePos              :: !(Double, Double)
    , _stateMouseButtonState      :: !MouseButtonState
    , _stateFrameCounter          :: !Double
    , _stateCurrentSysTime        :: !SystemTime
                                    -- Time, function taking time difference, 
    , _stateInterpolators         :: ![Interpolator]
    , _stateMouseEvents           :: !MouseEvents
    }

type Demo = RWST Env () State IO


data Interpolator = Interpolator
    {
      _interpolatorStartTime :: !SystemTime
    , _interpolatorDuration  :: !Float
    , _interpolatorCallback  :: !(Linear.V2 Float -> Demo ())
    , _interpolatorFunction  :: !(Float -> Linear.V2 Float)
    }

data MouseButtonState = PressDown | Pressed | Released | None
    deriving (Show, Eq)

data MouseEvents = MouseEvents
    {
      _onMiceClicks   :: [Double -> Double -> Demo ()]
    , _onMicePressed  :: [Double -> Double -> Demo ()]
    , _onMiceReleased :: [Double -> Double -> Demo ()]
    }

$(makeLenses ''MouseEvents)
$(makeLenses ''State)

--------------------------------------------------------------------------------

getFromHashMap:: UUID -> Demo (Maybe RawAndRenderedObjectPair)
getFromHashMap uuid = do
    interactable <- _stateInteractable <$> get

    return $ HM.lookup uuid interactable

insertToHashMap:: RawAndRenderedObjectPair -> Demo ()
insertToHashMap obj = do
    interactable <- _stateInteractable <$> get
    let uuid = obj ^. (rawFromPair . objectToRenderUUID)

    modify $ \s -> s & stateInteractable .~ HM.alter (\_ -> Just obj) uuid interactable
    return()


proccessMouseEvents:: Demo ()
proccessMouseEvents = do
    miceEvents <- _stateMouseEvents <$> get
    mouseState <- _stateMouseButtonState <$> get
    (x,y) <- _stateMousePos <$> get

    case mouseState of
        PressDown -> mapM_ (\f -> f x y) $ miceEvents ^. onMiceClicks
        Pressed -> mapM_ (\f -> f x y) $ miceEvents ^. onMicePressed
        Released -> mapM_ (\f -> f x y) $ miceEvents ^. onMiceReleased
        _ -> return ()

    return ()

createState:: Int -> Int -> IO State
createState width height = do

    spatialMap <- liftIO $ SM.createIOSpatialMap (20, 20) (width, height)
    sysTime <- getSystemTime

    let mouseEvents = MouseEvents {
          _onMiceClicks   = []
        , _onMicePressed  = []
        , _onMiceReleased = []
    }

    return State
        { _stateWindowWidth           = width
        , _stateWindowHeight          = height
        , _stateElementsOnScreen      = spatialMap
        , _stateRenderedCache         = V.empty
        , _stateInteractable          = HM.empty
        , _stateMousePos              = (0,0)
        , _stateMouseButtonState      = None
        , _stateFrameCounter          = 0
        , _stateCurrentSysTime        = sysTime
        , _stateInterpolators         = []
        , _stateMouseEvents           = mouseEvents
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

runInterpolatorHasElapsed:: Interpolator -> SystemTime -> Demo Bool
runInterpolatorHasElapsed i currentTime = do
    let Interpolator {
          _interpolatorCallback  = cb
        , _interpolatorDuration  = durationTime
        , _interpolatorStartTime = startTime
        , _interpolatorFunction  = f
    } = i

    let MkSystemTime {
          systemSeconds = curSecs
        , systemNanoseconds = curNanos
    } = currentTime

    let MkSystemTime {
          systemSeconds = startSecs
        , systemNanoseconds = startNanos
    } = startTime

    let startTimeInSecs = ((fromIntegral startSecs) + (fromIntegral startNanos) * 1E-9) :: Double
    let currentTimeInSecs = ((fromIntegral curSecs) + (fromIntegral curNanos) * 1E-9) :: Double

    let elapsedMs = (currentTimeInSecs - startTimeInSecs) * 1000

    -- let elapsedMs = (elapsedSecs * 1000) + (fromIntegral elapsedNanos `div` 1000000)
    -- let durationTime = MkSystemTime {
    --       systemSeconds = floor $ dur / 1000
    --     , systemNanoseconds = (castFloatToWord32 dur `mod` 1000) * 10E9
    -- }

    let t = min 1 (double2Float elapsedMs / durationTime)

    let currentValue = f t
    cb currentValue

    return $ t == 1


interpolate:: Float -> Linear.V2 Float -> Linear.V2 Float -> (Linear.V2 Float -> Demo ()) -> (Float -> Linear.V2 Float) -> Demo ()
interpolate timeLength a b lerpCallback diffFunction = do
    startTime <- _stateCurrentSysTime <$> get
    let i = Interpolator {
              _interpolatorCallback  = lerpCallback
            , _interpolatorDuration  = timeLength
            , _interpolatorStartTime = startTime
            , _interpolatorFunction  = diffFunction
        }

    modify $ \s -> s & stateInterpolators %~ (:) i

-- in miliseconds
lerp2, qerp2:: Float -> Linear.V2 Float -> Linear.V2 Float -> (Linear.V2 Float -> Demo ()) -> Demo ()
lerp2 timeLength a b lerpCallback = do
    let lerpInterpolator timeDifference = a + timeDifference Linear.*^ (b-a)
    interpolate timeLength a b lerpCallback lerpInterpolator

qerp2 timeLength a b lerpCallback = error "Qerp not implemented"


-- lerp2:: Float -> Linear.V2 Float -> Linear.V2 Float -> (Float -> Demo ()) -> Demo ()
-- lerp2 timeLength a b lerpCallback = do

processInterpolations:: Demo ()
processInterpolations = do
    interps <- _stateInterpolators <$> get

    currentTime <- _stateCurrentSysTime <$> get

    newInterps <- foldrM (\interp interpAcc-> do
        hasElapsed <- runInterpolatorHasElapsed interp currentTime
        return $ if hasElapsed
            then []
            else interp:interpAcc
        ) [] interps

    modify $ \s -> s & stateInterpolators .~ newInterps

    return ()

startup:: Demo ()
startup = do
    state <- get
    let width  = _stateWindowWidth  state
        height = _stateWindowHeight state


    let conf = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 0 1)}
    let thing = Circle{_circlePosition= Linear.V2 100 800, _diameter = 100 }

    program <- askForShader "BasicShaders"

    cache <- liftIO $ bindToGL thing conf program

    uuid1 <- liftIO nextRandom
    let objectToRender = ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive thing
        ,   _objectToRenderConfig = conf
        ,   _objectToRenderIndexWithinBuffer = 1
        ,   _objectToRenderProgram = program
        ,   _objectToRenderUUID = uuid1
    }

    let conf2 = emptyConfig {_m_backgroundColor = Just $ RGBA (Linear.V4 0 1 1 1)}
    let thing2 = Circle{_circlePosition= Linear.V2 300 100, _diameter = 80 }

    cache2 <- liftIO $ bindToGL thing2 conf2 program

    uuid2 <- liftIO nextRandom
    let objectToRender2 = ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive thing2
        ,   _objectToRenderConfig = conf2
        ,   _objectToRenderIndexWithinBuffer = 1
        ,   _objectToRenderProgram = program
        ,   _objectToRenderUUID = uuid2
    }

    let pair = RawAndRenderedObjectPair objectToRender cache
    let pair2 = RawAndRenderedObjectPair objectToRender2 cache2


    let recs = map (\(x, y) -> Rectangle (Linear.V2 x y) 10 10) [(x,y)| y <- [10,30..120],  x <- [10,30..120]]

    programInstanced <- askForShader "InstancedShaders"

    cache3 <- liftIO $ bindInstancedToGL recs conf2 programInstanced

    pair3 <- imapM (\i rect -> do 
        uuid3 <- liftIO nextRandom
        return $ RawAndRenderedObjectPair ObjectToRender {
            _objectToRenderPrimitiveObject = toPrimitive rect
        ,   _objectToRenderConfig = conf2
        ,   _objectToRenderIndexWithinBuffer = i
        ,   _objectToRenderUUID = uuid3
        ,   _objectToRenderProgram = programInstanced
    } cache3) recs

    isCancelledRef <- liftIO $ newIORef False
    let someOnRelease x y = do
            m_val <- getFromHashMap uuid2
            let m_obj = (_objectToRenderPrimitiveObject . _rawFromPair) <$> m_val 
            let defaultOrAlreadyExisting = fromMaybe (toPrimitive thing2) m_obj
            let floatX = double2Float x 
            let floatY = double2Float y

            when (isPointInsidePolygon (defaultOrAlreadyExisting) (floatX, floatY)) $ do
                liftIO $ writeIORef isCancelledRef False
                lerp2 1000 (Linear.V2 floatX floatY) (Linear.V2 (floatX) (floatY + 100)) $ \(Linear.V2 a b) -> do
                    isCancelled <- liftIO $ readIORef isCancelledRef
                    unless isCancelled $ do 
                        let conf = emptyConfig { _m_translate = Just $ (Linear.V3 a b 0)}
                        let v = pair2 & (rawFromPair . objectToRenderPrimitiveObject) .~ applyConfig (toPrimitive thing2) conf
                        insertToHashMap v 
                        -- let iWithinBuffer = obj ^. objectToRenderIndexWithinBuffer
                        liftIO $ (cache2 ^. renderedObjectBindConfig) 1 conf
                        return ()
            return ()

    let someOnPress x y = do
            m_val <- getFromHashMap uuid2
            let m_obj = (_objectToRenderPrimitiveObject . _rawFromPair) <$> m_val 
            let defaultOrAlreadyExisting = fromMaybe (toPrimitive thing2) m_obj

            when (isPointInsidePolygon (defaultOrAlreadyExisting) (double2Float x, double2Float y)) $ do
                liftIO $ writeIORef isCancelledRef True
                let conf = emptyConfig { _m_translate = Just $ Linear.V3 (double2Float x) (double2Float y) 0 }
                let v = pair2 & (rawFromPair . objectToRenderPrimitiveObject) .~ applyConfig (toPrimitive thing2) conf
                insertToHashMap v 
                -- let iWithinBuffer = obj ^. objectToRenderIndexWithinBuffer
                liftIO $ (cache2 ^. renderedObjectBindConfig) 1 conf
                return ()
            return ()

                    --  & stateInteractable .~ M.fromList [pair, pair2] ++ pair3
    modify $ \s -> s & stateRenderedCache .~ V.fromList [cache, cache2, cache3]
                     & (stateMouseEvents . onMicePressed) %~ (:) someOnPress
                     & (stateMouseEvents . onMiceReleased) %~ (:) someOnRelease

    insertToHashMap pair
    insertToHashMap pair2
    mapM_ insertToHashMap pair3

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
    sysTime <- liftIO getSystemTime
    modify $ \s -> s & stateFrameCounter %~ (+) 1
                     & stateCurrentSysTime .~ sysTime

    win <- asks envWindow

    cache <- _stateRenderedCache <$> get

    handleMouseEvent

    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    mapM_ draw cache

    liftIO $ do
        GLFW.swapBuffers win
        GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents
    processInterpolations

    proccessMouseEvents
    handleMouseButton
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

setMousePos:: Double -> Double -> Demo ()
setMousePos x y = do
    modify $ \s -> s & stateMousePos .~ (x,y)

handleMouseEvent:: Demo ()
handleMouseEvent = do
    (x,y) <- _stateMousePos <$> get
    interactable <- _stateInteractable <$> get
    -- newInteractable <- forM interactable $ \raw -> do
    --     let obj = raw ^. rawFromPair
    --     let d = obj ^. objectToRenderPrimitiveObject

    --     let conf = emptyConfig {
    --         -- _m_backgroundColor = Just $ RGBA (Linear.V4 0 0 1 1), 
    --         _m_backgroundColor = Just $ RGBA (Linear.V4 1 1 0 1),
    --         _m_translate = Just $ Linear.V3 (double2Float x) (double2Float y) 0 }
    --     if (isPointInsidePolygon (d) (double2Float x, double2Float y)) then do
    --     -- liftIO $ print x
    --         let v = raw & (rawFromPair . objectToRenderPrimitiveObject) .~ applyConfig d conf
    --         let iWithinBuffer = obj ^. objectToRenderIndexWithinBuffer
    --         liftIO $ (raw ^. renderedFromPair ^. renderedObjectBindConfig) iWithinBuffer conf
    --         return v
    --     else 
    --         return raw 

    -- modify $ \x -> x & stateInteractable .~ newInteractable

    return ()

handleMouseButton:: Demo()
handleMouseButton = do
    mouseState <- _stateMouseButtonState <$> get
    case mouseState of
        Released -> modify $ \s -> s & stateMouseButtonState .~ None
        PressDown -> modify $ \s -> s & stateMouseButtonState .~ Pressed
        _ -> return ()

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
        -- screenHeight <- _stateWindowHeight <$> get
        -- grid <- _stateElementsOnScreen <$> get

        setMousePos x y
        -- val <- lookupValueFromTree (floor x,screenHeight - floor y)

        return ()

      (EventKey win k scancode ks mk) -> do
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True

      (EventMouseButton _ mb mba mk) -> do
        when (mb == GLFW.MouseButton'1) $ do
            case mba of
                GLFW.MouseButtonState'Pressed -> modify $ \s -> s & stateMouseButtonState .~ PressDown
                GLFW.MouseButtonState'Released -> modify $ \s -> s & stateMouseButtonState .~ Released

            return ()

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

    let proj = Linear.ortho (0) ( (int2Float w)) (int2Float h) (0) (int2Float (-w)) (int2Float w)
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
    -- val <- mapM (\a@SM.SpatialData{SM._spatialData = x} -> do
    --     let r = x ^. renderedFromPair ^. renderedObjectBuffers
    --     let obj = x ^. rawFromPair
    --     let d = obj ^. objectToRenderPrimitiveObject
    --     let indexInBuffer = obj ^. objectToRenderIndexWithinBuffer
    --     let primitive = obj ^. objectToRenderPrimitiveObject
    --     when (isPointInsidePolygon d (int2Float px, int2Float py)) $ do
    --         let conf = emptyConfig {
    --             _m_backgroundColor = Just $ RGBA (Linear.V4 0 0 1 1), 
    --             _m_translate = Just $ Linear.V3 (toEnum px) (toEnum py) 0 }
    --         liftIO $ (x ^. renderedFromPair ^. renderedObjectBindConfig) conf 


    --     return x
    --     ) vals
    -- return val
    return []

draw:: RenderedObject -> Demo ()
draw renderedObject = do
    width <- _stateWindowWidth <$> get
    height <- _stateWindowHeight <$> get
    let vao = renderedObject ^. (renderedObjectBuffers . glBuffersObject)
    let program = renderedObject ^. renderedObjectProgram
    liftIO $ do
        GL.currentProgram GL.$= Just program

    setViewProjectionMatrix program

    liftIO $ do
        GL.bindVertexArrayObject GL.$= Just vao
        renderedObject ^. renderedObjectDrawCall
        GL.bindVertexArrayObject GL.$= Nothing

    return ()
