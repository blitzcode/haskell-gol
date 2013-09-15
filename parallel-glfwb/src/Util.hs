
module Util (modifyEnvMVar, readEnvMVar, wrapSucc, wrapPred, whileM_, showDigitGroupSep) where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar_)
import Data.List (transpose)

-- When we have a monad that embeds both IO and Reader in its transformer stack, we can use
-- these helpers to easily read or modify the contents of an MVar inside the environment
modifyEnvMVar :: (MonadReader e m, MonadIO m) => (e -> MVar a) -> (a -> IO a) -> m ()
modifyEnvMVar fask fmod = do
    mvar <- asks fask
    liftIO $ modifyMVar_ mvar fmod 
readEnvMVar :: (MonadReader e m, MonadIO m) => (e -> MVar a) -> m a
readEnvMVar fask = do
    mvar <- asks fask
    liftIO $ readMVar mvar

-- Move through an enumeration, but wrap around when hitting the end instead of an runtime error
wrapSucc :: (Enum a, Bounded a, Eq a) => a -> a
wrapSucc a | a == maxBound = minBound
           | otherwise = succ a
wrapPred :: (Enum a, Bounded a, Eq a) => a -> a
wrapPred a | a == minBound = maxBound
           | otherwise = pred a

-- Exists in Control.Monad.Loops, which is unfortunately not part of the Haskell Platform
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
    x <- p
    when x $ f >> whileM_ p f

-- Add a comma separator every three digits
showDigitGroupSep :: Int -> String
showDigitGroupSep num = clean . interleave $ [separators, str]
    where str        = show num
          interleave = concat . transpose
          clean      = dropWhile (== ',') . filter (/= ' ')
          separators = map (\x -> if x `mod` 3 == 0 then ',' else ' ') $ reverse [1..length str]

