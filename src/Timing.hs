
module Timing (timedLoop, getCurTick) where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (when)

-- TODO: Make sure these timing functions actually worked and are not foiled by lazy evaluation in some way
--       Also, look at various other timing packages for comparison:
--       http://hackage.haskell.org/packages/archive/repa-io/latest/doc/html/Data-Array-Repa-IO-Timing.html
--       http://hackage.haskell.org/packages/archive/timeit/1.0.0.0/doc/html/System-TimeIt.html

timedLoop :: MonadIO m => (Double -> Double -> m Bool) -> m ()
timedLoop f = loop 0.0
    where
        loop lastTick = do
            -- Can't just use the GLFW timer "Just tick <- liftIO GLFW.getTime" here as we
            -- might be shutting down GLFW in the main thread while this is still running
            tick <- liftIO getCurTick
            quit <- tick `seq` f lastTick tick
            when quit $ loop tick

getCurTick :: IO Double
getCurTick = do
    tickUCT <- getCurrentTime
    -- Microsecond precision, should be fine with a Double considering the
    -- number of seconds in a day
    return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)

