
{-# LANGUAGE PackageImports, FlexibleContexts #-}

module Main (main) where

import Grid
import Util
import qualified Font
import qualified Timing
import qualified Patterns as Pat
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Control.Concurrent (forkIO, getNumCapabilities, setNumCapabilities, threadDelay)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Monad.RWS.Strict (RWST, asks, evalRWST, gets, modify, liftIO)
import Control.Exception (assert)
import System.Random (randoms, newStdGen, RandomGen)
import Text.Printf (printf)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU (ortho2D)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified GHC.Conc (getNumProcessors)

data Algorithm = Serial | ASync | CppSerial | CppPThread | RepaNOP | RepaTraverse | RepaStatStncl |
                 RepaConvolve | RepaPartition
                     deriving (Show, Eq, Enum, Bounded)

data Env = Env
    { envEventsChan   :: TQueue Event
    , envWindow       :: !GLFW.Window
    , envGridMV       :: MVar Grid
    , envGridWidth    :: !Int
    , envGridHeight   :: !Int
    , envFontTex      :: !GL.TextureObject
    , envGenMV        :: MVar Int
    , envGPSHistoryMV :: MVar [Double]
    , envAlgorithmMV  :: MVar Algorithm
    }

data State = State
    { stateFPSHistory :: ![Double]
    }

type SimDraw = RWST Env () State IO

main :: IO ()
main = do
    let gridWidth  = 256
        gridHeight = 256
        statusOffs = 35
    runOnAllCores
    eventsChan   <- newTQueueIO :: IO (TQueue Event)
    gridMV       <- newMVar . centerFromASCII gridWidth gridHeight $ Pat.getPattern Pat.Acorn
    genMV        <- newMVar 0
    gpsHistoryMV <- newMVar []
    algorithmMV  <- newMVar RepaPartition
    withWindow gridWidth (gridHeight + statusOffs) "Game of Life" $ \window -> do
        GLFW.setErrorCallback        $ Just $ errorCallback eventsChan
        GLFW.setKeyCallback   window $ Just $ keyCallback   eventsChan
        GLFW.swapInterval 0
        setup2DOpenGL gridWidth (gridHeight + statusOffs)
        Font.withFontTexture $ \fontTex -> do
            let env = Env
                    { envEventsChan   = eventsChan
                    , envWindow       = window
                    , envGridMV       = gridMV
                    , envGridWidth    = gridWidth
                    , envGridHeight   = gridHeight
                    , envFontTex      = fontTex
                    , envGenMV        = genMV
                    , envGPSHistoryMV = gpsHistoryMV
                    , envAlgorithmMV  = algorithmMV
                    }
                state = State
                    { stateFPSHistory = []
                    }
            _ <- forkIO $ runReaderT simulateThread env
            void $ evalRWST run env state

-- Tweak Me: On systems with hyperthreading, the best setting depends on the algorithm.
--
-- Would love to make his configurable at runtime, but currently there are
-- crashes with increasing it, and decreasing doesn't seem to work at all
runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities -- Why is that not the default?

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    True <- GLFW.init 
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    Just window <- GLFW.createWindow w h title Nothing Nothing
    GLFW.makeContextCurrent $ Just window
    f window
    GLFW.setErrorCallback $ Just simpleErrorCallback
    GLFW.destroyWindow window
    GLFW.terminate
    where
        simpleErrorCallback e s = putStrLn $ show e ++ " " ++  show s

setup2DOpenGL :: Int -> Int -> IO ()
setup2DOpenGL w h = do
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GLU.ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)
    -- Magic number working for NV & ATI
    GL.translate (GL.Vector3 0.375 0.375 0.0 :: GL.Vector3 GL.GLfloat)

data Event =
      EventError !GLFW.Error !String
    | EventKey   !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys

errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s            = atomically $ writeTQueue tc $ EventError e s
keyCallback   :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback   tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk

getGrid :: MonadReader Env m => m (MVar Grid, Int, Int) -- Need 'FlexibleContexts' for this to work
getGrid = do
    gridMV <- asks envGridMV
    w      <- asks envGridWidth
    h      <- asks envGridHeight
    return (gridMV, w, h)

simulateThread :: ReaderT Env IO ()
simulateThread = do
    (_, w, h) <- getGrid
    Timing.timedLoop $ \lastTick tick -> do
        -- Tweak me: Too many steps at once and we can't draw often enough, too
        --           few and the grid conversion / PThread overhead dominates
        let cppnsteps = 31
            defUpdate = updateHistory tick lastTick 1
        algo <- readEnvMVar envAlgorithmMV
        case algo of
            Serial ->
                -- Strict MVars are not yet in the Haskell Platform, but $! seems to do the job
                modifyEnvMVar envGridMV (\grid -> return $! stepGrid w h grid)                  >> defUpdate
            ASync ->
                modifyEnvMVar envGridMV (\grid -> stepGridAsync w h grid)                       >> defUpdate
            RepaNOP ->
                modifyEnvMVar envGridMV (\grid -> return $! stepGridRepaNOP           w h grid) >> defUpdate
            RepaTraverse ->
                modifyEnvMVar envGridMV (\grid -> return $! stepGridRepaTraverse      w h grid) >> defUpdate
            RepaStatStncl ->
                modifyEnvMVar envGridMV (\grid -> return $! stepGridRepaStaticStencil w h grid) >> defUpdate
            RepaConvolve ->
                modifyEnvMVar envGridMV (\grid -> return $! stepGridRepaConvolve      w h grid) >> defUpdate
            RepaPartition ->
                modifyEnvMVar envGridMV (\grid -> return $! stepGridRepaPartitioned   w h grid) >> defUpdate
            CppSerial -> do
                modifyEnvMVar envGridMV (\grid -> stepGridCpp cppnsteps 0 w h grid)
                updateHistory tick lastTick cppnsteps
            CppPThread -> do
                -- Tweak me: Haskell is generally recommended to one-OS-thread-per-core,
                -- the C++ code can benefit from hyperthreading
                nthreads <- liftIO getNumCapabilities
                modifyEnvMVar envGridMV (\grid -> stepGridCpp cppnsteps nthreads w h grid)
                updateHistory tick lastTick cppnsteps
        return True

updateHistory :: Double -> Double -> Int -> ReaderT Env IO ()
updateHistory tick lastTick nsteps = do
    modifyEnvMVar envGPSHistoryMV (\hist ->
        return $! take (500 `div` nsteps) $ 1.0 / ((tick - lastTick) / fromIntegral (nsteps)) : hist)
    modifyEnvMVar envGenMV (\gen ->
        return $! gen + nsteps)

run :: SimDraw ()
run = Timing.timedLoop $ \lastTick tick -> do
    modify $ \s -> s
        { stateFPSHistory = take 60 $ 1.0 / (tick - lastTick) : stateFPSHistory s }
    draw
    window <- asks envWindow
    liftIO $ do
        GLFW.swapBuffers window
        -- Bizarre isssue: Calling finish here causes the worker thread to run
        -- in lockstep with the main drawing one on some systems (NVIDIA laptop
        -- no, AMD desktop yes). No idea why this happens...
        -- GL.finish
        GLFW.pollEvents
        err <- GL.get GL.errors
        mapM_ print err
        -- Turning VSync on seems to be broken on some OS / GPU combos, just use
        -- this to make sure we're not burning cycles drawing above refresh rate
        whileM_ ((\cur -> (cur - tick) < (1.0 / 60.0)) <$> Timing.getCurTick) (threadDelay 1000)
    processEvents
    liftIO $ not <$> GLFW.windowShouldClose window

draw :: SimDraw ()
draw = do
    (gridMV, w, h) <- getGrid
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 0.2 0.2 0.2 0.0 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer]
        GL.windowPos (GL.Vertex2 0 0 :: GL.Vertex2 GL.GLint)
        grid <- readMVar gridMV
        let toPixels = (\x -> if x then 0x00FFFFFF else 0x00000000) :: Bool -> Word32
            arr = VS.convert $ VU.map toPixels grid
            size = GL.Size (fromIntegral w) (fromIntegral h)
        assert (w * h == VS.length arr) $ VS.unsafeWith arr (\ptr ->
            GL.drawPixels size (GL.PixelData GL.RGBA GL.UnsignedByte ptr))
    drawStats

drawStats :: SimDraw ()
drawStats = do
    fpsHist   <- gets stateFPSHistory
    gen       <- readEnvMVar envGenMV
    gpsHist   <- readEnvMVar envGPSHistoryMV
    algo      <- readEnvMVar envAlgorithmMV
    ncap      <- liftIO getNumCapabilities
    (_, w, h) <- getGrid
    let avgFPS = sum fpsHist / fromIntegral (length fpsHist)
        avgGPS = sum gpsHist / fromIntegral (length gpsHist)
        str    = printf "%5i"   gen    ++   "G | " ++
                 printf "%4.0f" avgGPS ++ "GPS | " ++
                 printf "%4.1f" avgFPS ++ "FPS | " ++
                 "Pat [RGAOSK]\n" ++
                 printf "Grid: %ix%i | GPS * Area: %s \n"
                     w h (showDigitGroupSep (round $ fromIntegral w * fromIntegral h * avgGPS :: Int)) ++
                 printf "%i Threads | [C]ode (%i/%i): "
                     ncap
                     (fromEnum algo + 1 :: Int)
                     (fromEnum (maxBound :: Algorithm) + 1) ++
                 show algo
    fontTex <- asks envFontTex
    height  <- asks envGridHeight
    liftIO $ Font.drawText fontTex 3 (height + 24) 0x0000FF00 str

processEvents :: SimDraw ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
        Just e -> do
            processEvent e
            processEvents
        Nothing -> return ()

processEvent :: Event -> SimDraw ()
processEvent ev =
    case ev of
        (EventError e s) -> do
            window <- asks envWindow
            liftIO $ do
                putStrLn $ "error " ++ show e ++ " " ++ show s
                GLFW.setWindowShouldClose window True
        (EventKey window k _ ks mk) ->
            when (ks == GLFW.KeyState'Pressed) $ do
                when (k == GLFW.Key'Escape) $
                    liftIO $ GLFW.setWindowShouldClose window True
                when (k == GLFW.Key'R) $ do
                    (_, w, h) <- getGrid
                    modifyEnvMVar envGridMV (const          $! makeRandomGridIO w h)
                    modifyEnvMVar envGenMV  (const . return $! 0)
                when (k == GLFW.Key'A) . updateFromASCII $ Pat.getPattern Pat.Acorn
                when (k == GLFW.Key'G) . updateFromASCII $ Pat.getPattern Pat.Gun
                when (k == GLFW.Key'O) . updateFromASCII $ Pat.getPattern Pat.OrientTest
                when (k == GLFW.Key'S) . updateFromASCII $ Pat.getPattern Pat.Spacefill
                when (k == GLFW.Key'K) . updateFromASCII $ Pat.getPattern Pat.Ark
                when (k == GLFW.Key'C) $ do
                    modifyEnvMVar envAlgorithmMV $ \x ->
                        return $ (if GLFW.modifierKeysShift mk then wrapPred else wrapSucc) x
                    modifyEnvMVar envGPSHistoryMV $ const (return []) -- Clear history on algorithm switch

makeRandomGridIO :: Int -> Int -> IO Grid
makeRandomGridIO w h = do 
    gen <- newStdGen
    return $ makeRandomGrid w h gen
makeRandomGrid :: RandomGen g => Int -> Int -> g -> Grid
makeRandomGrid w h g = VU.fromListN (w * h) rndList
    where rndList = map (0.75 <) (randoms g :: [Float])

gridFromASCII :: Int -> Int -> Int -> Int -> [String] -> Grid
gridFromASCII w h xoffs yoffs str = arr . zip idx . asc $ reverse str
    where idx = [(x + xoffs - awdh `div` 2) + (y + yoffs - ahgt `div` 2) * w |
                 y <- [0..ahgt - 1],
                 x <- [0..awdh - 1]]
              where ahgt = length str
                    awdh = length . head $ str
          asc = concatMap (map fromASCII)
              where fromASCII x = case x of { 'O' -> True; _ -> False }
          arr = (VU.//) $ VU.replicate (w * h) False

centerFromASCII :: Int -> Int -> [String] -> Grid 
centerFromASCII w h = gridFromASCII w h (w `quot` 2) (h `quot` 2)
updateFromASCII :: [String] -> SimDraw () 
updateFromASCII str = do
    (_, w, h) <- getGrid
    modifyEnvMVar envGridMV (const . return $! centerFromASCII w h str)
    modifyEnvMVar envGenMV  (const . return $! 0)

