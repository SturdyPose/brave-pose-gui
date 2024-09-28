{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE TemplateHaskell #-}
module Inputs.MouseHandling where
import Graphics.Primitives
import GHC.Float (int2Float)
import qualified Linear
import Control.Monad.RWS.Strict 
import qualified Data.Map.Strict as M
import Control.Lens 
import StrictTypes.Tuples (Pair (..))

data MouseButtonState = PressDown | Pressed | Released | None
    deriving (Show, Eq)

data MouseButton = Left | Right | Middle | Other Int

data ModifierKeys = ModifierKeys {
      _mouseHandlingAltPressed                       :: !Bool
    , _mouseHandlingCtrlPressed                      :: !Bool
    , _mouseHandlingShiftPressed                     :: !Bool
    , _mouseHandlingSuperPressed                     :: !Bool
} deriving Show

data MouseHandlingEnv m a = MouseHandlingEnv
    {
          _mouseHandlingEnvMouseCoordinates              :: !(Pair Int Int)
        , _mouseHandlingEnvMouseCoordinatesPreviousFrame :: !(Pair Int Int)
        , _mouseHandlingEnvInteractables                 :: !(M.Map String (Interactable a, MouseHandlingEvents m a))
        , _mouseHandlingMouseButtonState                 :: !MouseButtonState
        , _mouseHandlingModifierKeys                     :: !ModifierKeys
        , _mouseHandlingActivated                        :: !Bool
    }

data Interactable a where
    Interactable ::
        PolygonShape a =>
        {
              interactableElement:: a
            , interactableId     :: String 
        }
        -> Interactable a

type MouseHandling m a = (Int, Int) -> ModifierKeys -> (a, MouseHandlingEvents m a) -> String -> m ()

data MouseHandlingEvents m a = MouseHandlingEvents
    {
          _onMiceClicks   :: !(MouseHandling m a)
        , _onMiceDown     :: !(MouseHandling m a)
        , _onMicePressed  :: !(MouseHandling m a)
        , _onMiceUp       :: !(MouseHandling m a)
        , _onMiceOver     :: !(MouseHandling m a)
        , _onMiceEnter    :: !(MouseHandling m a)
        , _onMiceLeave    :: !(MouseHandling m a)
        , _onMiceMove     :: !(MouseHandling m a)
    } 

data MouseHandlingState a = MouseHandlingState
    {
        _mouseHandlingStateActiveInteractables :: !(M.Map String (Interactable a))
      , _mouseHandlingPreviousMouseState       :: !MouseButtonState
    }

$(makeLenses ''MouseHandlingState)

type MouseHandlingPlugin m a = RWST (MouseHandlingEnv m a) () (MouseHandlingState a) m


pureHandle :: Monad m => MouseHandling m a 
pureHandle _ _ _ _ = pure ()


-- What should happen to mouse if state changed between frames?
-- prevFrameMouse is first arg and current frame mouse is second arg 
mouseStateOnThisFrame:: MouseButtonState -> MouseButtonState -> MouseButtonState
mouseStateOnThisFrame PressDown PressDown = Pressed
mouseStateOnThisFrame Released Released = None 
mouseStateOnThisFrame _ x = x 

emptyEvent:: Monad m => m (MouseHandlingEvents m a)
emptyEvent = return $ MouseHandlingEvents {
      _onMiceClicks   = pureHandle
    , _onMiceDown     = pureHandle
    , _onMicePressed  = pureHandle
    , _onMiceUp       = pureHandle
    , _onMiceOver     = pureHandle
    , _onMiceEnter    = pureHandle
    , _onMiceLeave    = pureHandle
    , _onMiceMove     = pureHandle
}

-- This is output to the env for the next frame
processMouseEvents:: (MonadIO m, PolygonShape a) => MouseHandlingPlugin m a ()
processMouseEvents = do
    mouseState <- _mouseHandlingMouseButtonState <$> ask 
    interactables <- _mouseHandlingEnvInteractables <$> ask
    modifierKeys <- _mouseHandlingModifierKeys <$> ask
    (Pair mouseX mouseY) <- _mouseHandlingEnvMouseCoordinates <$> ask 
    (Pair previousMouseX previousMouseY) <- _mouseHandlingEnvMouseCoordinatesPreviousFrame <$> ask 
    interactableMap <- _mouseHandlingStateActiveInteractables <$> get
    previousMouseButtonState <- _mouseHandlingPreviousMouseState <$> get

    let currentFrameMouseState = mouseStateOnThisFrame previousMouseButtonState mouseState

    forM_ interactables $ \p@(interactable, events) -> do
        let elem = interactableElement interactable
        let id = interactableId interactable

        let pointStillInPolygon = isPointInsidePolygon elem (int2Float mouseX, int2Float mouseY) 
        let previousPointStillInPolygon = isPointInsidePolygon elem (int2Float previousMouseX, int2Float previousMouseY)

        if pointStillInPolygon && not previousPointStillInPolygon then do
            -- Mouse entered the object
            lift $ (_onMiceEnter events) (mouseX, mouseY) modifierKeys (elem,events) id
            return ()
        else if pointStillInPolygon && previousPointStillInPolygon then do
            let isElementPresentInPreviousFrame = M.member id interactableMap
            -- Mouse is somewhere withing the object 
            when (mouseX /= previousMouseX || mouseY /= previousMouseY) $ 
                lift $ (_onMiceMove events) (mouseX, mouseY) modifierKeys (elem,events) id

            case currentFrameMouseState of 
                PressDown -> do 
                    modify $ \s -> s & mouseHandlingStateActiveInteractables %~ M.insert id interactable 
                    lift $ (_onMiceDown events) (mouseX, mouseY) modifierKeys (elem,events) id

                Pressed -> do 
                    when isElementPresentInPreviousFrame $ do
                        lift $ (_onMicePressed events) (mouseX, mouseY) modifierKeys (elem,events) id

                Released -> do 
                    lift $ (_onMiceUp events) (mouseX, mouseY) modifierKeys (elem,events) id
                    when isElementPresentInPreviousFrame $ do
                        lift $ (_onMiceClicks events) (mouseX, mouseY) modifierKeys (elem,events) id

                None -> return ()

            return ()
        else when (not pointStillInPolygon && previousPointStillInPolygon) $ do
            -- Mouse is leaving the object
            lift $ (_onMiceLeave events) (mouseX, mouseY) modifierKeys (elem,events) id

        -- Check if mouse quickly goes quickly through the object in single frame
        when (rayIntersectsPolygon elem (Linear.V2 (int2Float previousMouseX) (int2Float previousMouseY)) (Linear.V2 (int2Float mouseX) (int2Float mouseY))) $ do
            when (not pointStillInPolygon && not previousPointStillInPolygon) $ do
                -- TODO: Maybe queue on leave so it has time to render on next frame
                -- This needs some experimentation
                lift $ (_onMiceEnter events) (mouseX, mouseY) modifierKeys (elem,events) id
                lift $ (_onMiceLeave events) (mouseX, mouseY) modifierKeys (elem,events) id
        

        when (currentFrameMouseState == Released) $ do 
            modify $ \s -> s & mouseHandlingStateActiveInteractables .~ M.empty

        return ()
         
    modify $ \s -> s & mouseHandlingPreviousMouseState .~ mouseState
    return ()