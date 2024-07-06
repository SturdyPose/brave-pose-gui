{-# LANGUAGE TemplateHaskell #-}
module Interpolation.Interpolator where

import qualified Linear
    
import Data.Time.Clock.System
import Control.Lens ((%~), (&), makeLenses)
import Data.Foldable (foldrM)
import GHC.Float (double2Float)
import Control.Monad.State.Strict (lift)
import Control.Lens.Operators ((.~))
import Control.Monad.Trans.RWS.Strict


data Interpolator m = Interpolator
    {
      _interpolatorStartTime :: !SystemTime
    , _interpolatorDuration  :: !Float
    , _interpolatorCallback  :: !(Linear.V2 Float -> m ())
    , _interpolatorFunction  :: !(Float -> Linear.V2 Float)
    }

data InterpolationEnv = InterpolationEnv {
    _interpolationEnvTime          :: !SystemTime
}

data InterpolationState m = InterpolationState
    {
        _interpolationStateInterpolators :: ![Interpolator m]
    }

type InterpolationPlugin m a = RWST InterpolationEnv () (InterpolationState m) m a

$(makeLenses ''InterpolationEnv)
$(makeLenses ''InterpolationState)

-- updateTime:: Monad m => SystemTime -> InterpolationPlugin m ()
-- updateTime time = modify $ \s -> s & interpolationTime .~ time

createInterpolationState:: Monad m => InterpolationState m
createInterpolationState = 
    InterpolationState {
        _interpolationStateInterpolators = [] 
    }

addInterpolator:: Monad m => Interpolator m -> InterpolationPlugin m () 
addInterpolator i = modify $ \s -> s & interpolationStateInterpolators %~ (:) i

interpolate:: Monad m => Float -> (Linear.V2 Float -> m ()) -> (Float -> Linear.V2 Float) -> InterpolationPlugin m ()
interpolate timeLength lerpCallback diffFunction = do
    startTime <- _interpolationEnvTime <$> ask
    let i = Interpolator {
              _interpolatorCallback  = lerpCallback
            , _interpolatorDuration  = timeLength
            , _interpolatorStartTime = startTime
            , _interpolatorFunction  = diffFunction
        }

    modify $ \s -> s & interpolationStateInterpolators %~ (:) i
    return ()

runInterpolatorHasElapsed:: Monad m => Interpolator m -> SystemTime -> InterpolationPlugin m Bool
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

    let t = min 1 (double2Float elapsedMs / durationTime)

    let currentValue = f t
    lift $ cb currentValue

    return $ t == 1

processInterpolations:: Monad m => InterpolationPlugin m ()
processInterpolations = do
    interps <- _interpolationStateInterpolators <$> get

    currentTime <- _interpolationEnvTime <$> ask

    newInterps <- foldrM (\interp interpAcc-> do
        hasElapsed <- runInterpolatorHasElapsed interp currentTime
        return $ if hasElapsed
            then []
            else interp:interpAcc
        ) [] interps

    modify $ \s -> s & interpolationStateInterpolators .~ newInterps

    return ()

lerp:: Monad m => Float -> Float -> Float -> (Float -> m ()) -> InterpolationPlugin m ()  
lerp timeLength a b lerpCallback = do
    let lerpInterpolator timeDifference = Linear.V2 a 0 + timeDifference Linear.*^ (Linear.V2 b 0 - Linear.V2 a 0)
    interpolate timeLength (\(Linear.V2 x _) -> lerpCallback x) lerpInterpolator

-- in miliseconds
lerp2, qerp2:: Monad m => Float -> Linear.V2 Float -> Linear.V2 Float -> (Linear.V2 Float -> m ()) -> InterpolationPlugin m () 
lerp2 timeLength a b lerpCallback = do
    let lerpInterpolator timeDifference = a + timeDifference Linear.*^ (b-a)
    interpolate timeLength lerpCallback lerpInterpolator

qerp2 timeLength a b lerpCallback = error "Qerp not implemented"