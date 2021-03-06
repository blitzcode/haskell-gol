
{-# LANGUAGE PackageImports, FlexibleContexts, BangPatterns #-}

module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word32)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (ord)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Control.Monad.Trans (MonadIO)
import Control.Concurrent (forkIO, setNumCapabilities, threadDelay)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad.RWS.Strict (RWST, asks, evalRWST, gets, modify, liftIO)
import Control.Exception (assert)
import System.Random (randoms, newStdGen, RandomGen)
import Text.Printf (printf)
import qualified Foreign.Marshal.Array (withArray)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU (build2DMipmaps, ortho2D)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified GHC.Conc (getNumProcessors)

type Grid = VU.Vector Bool

data Env = Env
    { envEventsChan   :: TQueue Event
    , envWindow       :: !GLFW.Window
    , envGridMV       :: MVar Grid
    , envGridWidth    :: !Int
    , envGridHeight   :: !Int
    , envFontTex      :: !GL.TextureObject
    , envGenMV        :: MVar Int
    , envGPSHistoryMV :: MVar [Double]
    }

data State = State
    { stateFPSHistory :: ![Double]
    }

type SimDraw = RWST Env () State IO

main :: IO ()
main = do
    let gridWidth  = 256
        gridHeight = 256
        statusOffs = 11
    runOnAllCores
    eventsChan   <- newTQueueIO :: IO (TQueue Event)
    gridMV       <- newMVar $ centerFromASCII gridWidth gridHeight acorn
    genMV        <- newMVar $ 0
    gpsHistoryMV <- newMVar $ []
    withWindow gridWidth (gridHeight + statusOffs) "Game of Life" $ \window -> do
        GLFW.setErrorCallback        $ Just $ errorCallback eventsChan
        GLFW.setKeyCallback   window $ Just $ keyCallback   eventsChan
        GLFW.swapInterval 0
        setup2DOpenGL gridWidth (gridHeight + statusOffs)
        withFontTexture $ \fontTex -> do
            let env = Env
                    { envEventsChan   = eventsChan
                    , envWindow       = window
                    , envGridMV       = gridMV
                    , envGridWidth    = gridWidth
                    , envGridHeight   = gridHeight
                    , envFontTex      = fontTex
                    , envGenMV        = genMV
                    , envGPSHistoryMV = gpsHistoryMV
                    }
                state = State
                    { stateFPSHistory = []
                    }
            _ <- forkIO $ runReaderT simulateThread env
            void $ evalRWST run env state

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

withFontTexture :: (GL.TextureObject -> IO ()) -> IO ()
withFontTexture f = do
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D GL.$= Just tex
    -- Convert font grid bitmap image from Word32 list into byte array
    let fontImgArray = VU.fromListN (16 * 16 * 6 * 12 `div` 8) . 
                       concatMap (\x -> map (extractByte x) [0..3]) $ miscFixed6x12Data
                       :: VU.Vector Word8
    -- Extract bits (reversed in byte), store transparent / opaque pixels in square texture
    let fontTex = [toRGBA $ texel x y | y <- [0..fontTexWdh - 1], x <- [0..fontTexWdh - 1]] :: [Word32]
         where texel x y = (srcLookup x y .&. (1 `shiftL` (7 - (srcIdx x y `mod` 8))))
               srcLookup x y | (x < fontImgWdh && y < fontImgHgt) = fontImgArray VU.! (srcIdx x y `div` 8)
                             | otherwise                          = 0
               srcIdx x y = x + y * fontImgWdh
               toRGBA a = case a of { 0 -> 0x0FFFFFF; _ -> 0xFFFFFFFF }
    Foreign.Marshal.Array.withArray fontTex $ \ptr -> GLU.build2DMipmaps
        GL.Texture2D GL.RGBA' (fromIntegral fontTexWdh) (fromIntegral fontTexWdh)
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    f tex
    GL.deleteObjectNames [tex]

extractByte :: Word32 -> Int -> Word8
extractByte x i = fromIntegral $ (x .&. (0xFF `shiftL` (i * 8))) `shiftR` (i * 8)

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

timedLoop :: MonadIO m => (Double -> Double -> m Bool) -> m ()
timedLoop f = loop 0.0
    where
        loop lastTick = do
            -- Can't just use the GLFW timer "Just tick <- liftIO GLFW.getTime" here as we
            -- might be shutting down GLFW in the main thread while this one is still running
            tick <- liftIO getCurTick
            quit <- f lastTick tick
            when quit $ loop tick

getCurTick :: IO Double
getCurTick = do
    tickUCT <- getCurrentTime
    -- Microsecond precision, should be fine with a Double considering the
    -- number of seconds in a day
    return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)

getGrid :: MonadReader Env m => m (MVar Grid, Int, Int) -- Need 'FlexibleContexts' for this to work
getGrid = do
    gridMV <- asks envGridMV
    w      <- asks envGridWidth
    h      <- asks envGridHeight
    return (gridMV, w, h)

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

simulateThread :: ReaderT Env IO ()
simulateThread = do
    (_, w, h) <- getGrid
    timedLoop $ \lastTick tick -> do
        -- Strict MVars are not yet in the Haskell Platform, but $! seems to do the job. This
        -- completely blows up when the actual grid simulation is done lazy
        modifyEnvMVar envGPSHistoryMV (\hist -> return $! take 250 $ 1.0 / (tick - lastTick) : hist)
        modifyEnvMVar envGridMV       (\grid -> return $! stepGrid w h grid)
        modifyEnvMVar envGenMV        (\gen  -> return $! gen + 1)
        return True

run :: SimDraw ()
run = timedLoop $ \lastTick tick -> do
    modify $ \s -> s
        { stateFPSHistory = take 60 $ 1.0 / (tick - lastTick) : stateFPSHistory s
        }
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
        whileM_ ((\cur -> (cur - tick) < (1.0 / 60.0)) <$> getCurTick) (threadDelay 100)
    processEvents
    liftIO $ not <$> GLFW.windowShouldClose window

-- Exists in Control.Monad.Loops, which is unfortunately not part of the Haskell Platform
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
    x <- p
    when x $ do f >> whileM_ p f

-- TODO: Make me parallel! The standard parallel options seem like they would
--       either require us to give up mutable vectors (ST) or use IO, maybe
--       have a look at Repa...
stepGrid :: Int -> Int -> Grid -> Grid
stepGrid !w !h !g = VU.create $ do -- ST monad (need locally mutable vectors)
    arr <- VUM.new $ w * h
    -- Life rules
    let alive x y check = (nb == 3) || (cell && nb == 2)
         where idx = x + y * w
               cell = g `VU.unsafeIndex` idx
               nb = foldr (\i a -> a + if (g `VU.unsafeIndex` i) then 1 else 0) 0 nbList :: Int
               nbList | not check = [ idx + 1    , idx - 1           -- 1D faster in the interior
                                    , idx + w    , idx - w
                                    , idx + 1 + w, idx + 1 - w
                                    , idx - 1 + w, idx - 1 - w
                                    ]
                      | otherwise = map (\(x', y') -> (x' `mod` w) + (y' `mod` h) * w)
                                    [ (x + 1, y    ), (x    , y + 1) -- 2D faster for torus boundaries
                                    , (x - 1, y    ), (x    , y - 1)
                                    , (x + 1, y + 1), (x - 1, y - 1)
                                    , (x + 1, y - 1), (x - 1, y + 1)
                                    ]
    mapM_ (\(x, y, check) -> VUM.unsafeWrite arr (x + y * w) $ alive x y check) $
        -- Borders, make torus array
        [(x, y, True ) | y <- [0, h - 1], x <- [0..w - 1]] ++
        [(x, y, True ) | x <- [0, w - 1], y <- [0..h - 1]] ++
        -- Inner part of the grid, no need for bounds checking
        [(x, y, False) | y <- [1..h - 2], x <- [1..w - 2]]
    return arr

-- Before splitting the nbList for the wrap / nowrap case, slightly wrong wrapping behavior,
-- no argument strictness
{-
stepGrid :: Int -> Int -> Grid -> Grid
stepGrid w h g = runST $ do
    arr <- VUM.new (w * h)
    -- Life rules
    let alive idx check = (nb == 3) || (cell && nb == 2)
             where cell = g `VU.unsafeIndex` idx
                   nb = foldr (\i a -> a + if (g `VU.unsafeIndex` i) then 1 else 0) 0 nbList :: Int
                   nbList = map (if check then wrapGrid else id)
                            [ idx + 1    , idx - 1
                            , idx + w    , idx - w
                            , idx + 1 + w, idx + 1 - w
                            , idx - 1 + w, idx - 1 - w
                            ]
                       where wrapGrid i = (x `mod` w) + (y `mod` h) * w
                                where x = i `mod` w
                                      y = (i - x) `div` w
    mapM_ (\(x, y, check) -> VUM.unsafeWrite arr (x + y * w) $ alive (x + y * w) check) $
        -- Borders, make torus array
        [(x, y, True ) | y <- [0, h - 1], x <- [0..w - 1]] ++
        [(x, y, True ) | x <- [0, w - 1], y <- [0..h - 1]] ++
        -- Inner part of the grid, no need for bounds checking
        [(x, y, False) | y <- [1..h - 2], x <- [1..w - 2]]
    VU.unsafeFreeze arr
-}

-- Older version without ST monad based on immutable unboxed vectors
{-
stepGrid :: Int -> Int -> Grid -> Grid
stepGrid w h g = VU.replicate (w * h) False VU.//
    map (\(x, y) -> (x + y * w, alive (x + y * w) False)) [(x, y) | y <- [1..h - 2], x <- [1..w - 2]] VU.//
    map (\(x, y) -> (x + y * w, alive (x + y * w) True))
        ([(x, y) | y <- [0, h - 1], x <- [0..w - 1]] ++ [(x, y) | x <- [0, w - 1], y <- [0..h - 1]])
        where alive idx check = (cell && (nb == 2 || nb == 3)) || (not cell && nb == 3)
                where cell = g `VU.unsafeIndex` idx
                      nb = countBool . map (g `VU.unsafeIndex`) $ nbList
                      countBool = length . filter id
                      nbList = map (if check then wrapGrid else id) [idx + 1, idx - 1, idx + w, idx - w,
                                             idx + 1 + w, idx + 1 - w, idx - 1 + w, idx - 1 - w]
                          where wrapGrid i = (x `mod` w) + (y `mod` h) * w
                                   where x = i `mod` w
                                         y = (i - x) `div` w
-}

-- Older version using wrapping logic also for the interior
{-
stepGrid w h g = VU.imap alive g
    where alive idx cell = (cell && (nb == 2 || nb == 3)) || (not cell && nb == 3)
                where nb = countBool . map (g VU.!) $ nbList
                      countBool = length . filter id
                      nbList = map wrapGrid [idx + 1, idx - 1, idx + w, idx - w,
                                             idx + 1 + w, idx + 1 - w, idx - 1 + w, idx - 1 - w]
                          where wrapGrid i = (x `mod` w) + (y `mod` h) * w
                                   where x = i `mod` w
                                         y = (i - x) `div` w
-}

-- Older version based on unboxed 2D arrays (Data.Array.Unboxed)
{-
stepGrid g = listArray b $ map alive (indices g)
    where b@((x0, y0), (x1, y1)) = bounds g
          alive i@(ix, iy) = (cell && (nb == 2 || nb == 3)) || (not cell && nb == 3)
              where cell = g ! i
                    nb = countBool . map ({-# SCC "lookup" #-} (g !)) $ nbList
                    countBool = length . filter id
                    nbList = [wrapGrid (ix + x, iy + y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]
                        -- TODO: Assumes zero-based index
                        where wrapGrid (x, y) = (x `mod` (x1 - x0), y `mod` (y1 - y0)) 
-}

draw :: SimDraw ()
draw = do
    (gridMV, w, h) <- getGrid
    liftIO $ do
        GL.clear [GL.ColorBuffer]
        GL.windowPos (GL.Vertex2 0 0 :: GL.Vertex2 GL.GLint)
        grid <- readMVar gridMV
        let toPixels = (\x -> if x then 0x00FFFFFF else 0x00000000) :: Bool -> Word32
            arr = VS.convert $ VU.map toPixels grid
            size = GL.Size (fromIntegral w) (fromIntegral h)
        assert (w * h == VS.length arr) $ VS.unsafeWith arr (\ptr ->
            GL.drawPixels size (GL.PixelData GL.RGBA GL.UnsignedByte ptr))
    displayStats

displayStats :: SimDraw ()
displayStats = do
    fpsHist <- gets stateFPSHistory
    gen     <- readEnvMVar envGenMV
    gpsHist <- readEnvMVar envGPSHistoryMV
    let avgFPS = sum fpsHist / fromIntegral (length fpsHist)
        avgGPS = sum gpsHist / fromIntegral (length gpsHist)
        str    = printf "%5i"   gen    ++   "G | " ++
                 printf "%5.1f" avgGPS ++ "GPS | " ++
                 printf "%5.1f" avgFPS ++ "FPS | " ++
                 "Press RGAOS"
    fontTex <- asks envFontTex
    height  <- asks envGridHeight
    liftIO $ drawText fontTex 2 height 0x0000FF00 str

drawText :: GL.TextureObject -> Int -> Int -> Word32 -> String -> IO ()
drawText tex x y color str = do
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.textureFilter  GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
    GL.blend     GL.$= GL.Enabled  
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    mapM_ (\(pos, chr) -> do
        let xoffs          = pos * fontCharWdh
            idx            = ord chr
            tx             = (idx `mod` fontGridWdh);
            ty             = fontGridHgt - ((idx - (idx `mod` fontGridWdh)) `div` fontGridWdh + 1);
            ftx            = fromIntegral (tx * fontCharWdh) / fromIntegral fontTexWdh;
            fty            = fromIntegral (ty * fontCharHgt) / fromIntegral fontTexWdh;
            fontCharWdhTex = fromIntegral fontCharWdh / fromIntegral fontTexWdh
            fontCharHgtTex = fromIntegral fontCharHgt / fromIntegral fontTexWdh
        GL.renderPrimitive GL.Quads $ do
            let channel i = fromIntegral (extractByte color i) / 255.0
            GL.color (GL.Color3 (channel 0) (channel 1) (channel 2) :: GL.Color3 GL.GLfloat)
            GL.texCoord ((GL.TexCoord2
                (ftx)
                (fty)) :: GL.TexCoord2 GL.GLfloat)
            GL.vertex ((GL.Vertex2(
                (fromIntegral $ x + xoffs))
                (fromIntegral $ y        )) :: GL.Vertex2 GL.GLfloat)
            GL.texCoord ((GL.TexCoord2
                (ftx + fontCharWdhTex)
                (fty                 )) :: GL.TexCoord2 GL.GLfloat)
            GL.vertex ((GL.Vertex2(
                (fromIntegral $ x + xoffs + fontCharWdh))
                (fromIntegral $ y                      )) :: GL.Vertex2 GL.GLfloat)
            GL.texCoord ((GL.TexCoord2
                (ftx + fontCharWdhTex)
                (fty + fontCharHgtTex)) :: GL.TexCoord2 GL.GLfloat)
            GL.vertex ((GL.Vertex2(
                (fromIntegral $ x + xoffs + fontCharWdh))
                (fromIntegral $ y         + fontCharHgt)) :: GL.Vertex2 GL.GLfloat)
            GL.texCoord ((GL.TexCoord2
                (ftx                 )
                (fty + fontCharHgtTex)) :: GL.TexCoord2 GL.GLfloat)
            GL.vertex ((GL.Vertex2(
                (fromIntegral $ x + xoffs              ))
                (fromIntegral $ y         + fontCharHgt)) :: GL.Vertex2 GL.GLfloat)
        ) $ zip [0..] str
    GL.color (GL.Color3 1 1 1 :: GL.Color3 GL.GLfloat)
    GL.blend GL.$= GL.Disabled  
    GL.textureBinding GL.Texture2D GL.$= Nothing
    GL.texture GL.Texture2D GL.$= GL.Disabled

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
        (EventKey window k _ ks _) ->
            when (ks == GLFW.KeyState'Pressed) $ do
                when (k == GLFW.Key'Escape) $
                    liftIO $ GLFW.setWindowShouldClose window True
                when (k == GLFW.Key'R) $ do
                    (_, w, h) <- getGrid
                    modifyEnvMVar envGridMV (const          $! makeRandomGridIO w h)
                    modifyEnvMVar envGenMV  (const . return $! 0)
                when (k == GLFW.Key'A) $ updateFromASCII acorn
                when (k == GLFW.Key'G) $ updateFromASCII gun
                when (k == GLFW.Key'O) $ updateFromASCII orientTest
                when (k == GLFW.Key'S) $ updateFromASCII spacefill

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

acorn, gun, orientTest, spacefill :: [String]

acorn = [ ".O....."
        , "...O..."
        , "OO..OOO"
        ]

gun = [ "........................O..........."
      , "......................O.O..........."
      , "............OO......OO............OO"
      , "...........O...O....OO............OO"
      , "OO........O.....O...OO.............."
      , "OO........O...O.OO....O.O..........."
      , "..........O.....O.......O..........."
      , "...........O...O...................."
      , "............OO......................"
      ]

orientTest = [ "....OO...."
             , "....OO...."
             , ".........."
             , ".........."
             , "OO..OO..OO"
             , "OO..OO..OO"
             , ".........."
             , ".........."
             , "OO........"
             , "OO........"
             ]

-- http://www.radicaleye.com/lifepage/patterns/max.html
spacefill = [ ".....O.O....................."
            , "....O..O....................."
            , "...OO........................"
            , "..O.........................."
            , ".OOOO........................"
            , "O....O......................."
            , "O..O........................."
            , "O..O........................."
            , ".O.........OOO...OOO........."
            , "..OOOO.O..O..O...O..O........"
            , "...O...O.....O...O..........."
            , "....O........O...O..........."
            , "....O.O......O...O..........."
            , "............................."
            , "...OOO.....OOO...OOO........."
            , "...OO.......O.....O.........."
            , "...OOO......OOOOOOO.........."
            , "...........O.......O........."
            , "....O.O...OOOOOOOOOOO........"
            , "...O..O..O............OO....."
            , "...O.....OOOOOOOOOOOO...O...."
            , "...O...O.............O...O..."
            , "....O...OOOOOOOOOOOO.....O..."
            , ".....OO............O..O..O..."
            , "........OOOOOOOOOOO...O.O...."
            , ".........O.......O..........."
            , "..........OOOOOOO......OOO..."
            , "..........O.....O.......OO..."
            , ".........OOO...OOO.....OOO..."
            , "............................."
            , "...........O...O......O.O...."
            , "...........O...O........O...."
            , "...........O...O.....O...O..."
            , "........O..O...O..O..O.OOOO.."
            , ".........OOO...OOO.........O."
            , ".........................O..O"
            , ".........................O..O"
            , ".......................O....O"
            , "........................OOOO."
            , "..........................O.."
            , "........................OO..."
            , ".....................O..O...."
            , ".....................O.O....."
            ]

-- Bit packed font data for a 16 x 16 charcter grid of 6 x 12 pixel characters
fontGridWdh, fontGridHgt, fontImgWdh, fontImgHgt, fontCharWdh, fontCharHgt, fontTexWdh :: Int
fontGridWdh = 16
fontGridHgt = 16
fontImgWdh  = 96
fontImgHgt  = 192
fontCharWdh = 6
fontCharHgt = 12
fontTexWdh  = 256
miscFixed6x12Data :: [Word32]
miscFixed6x12Data =
    [ 0x00000000, 0x00000000, 0x20080200, 0x00000000, 0x00000000, 0x10080100, 0x711c2772, 0xc7f100c7
    , 0x088f701c, 0x8aa2288a, 0x28ca8828, 0x944889a2, 0x8aa2288a, 0x28aa8028, 0xa2288aa2, 0x8aa2288b
    , 0x289abe28, 0xa2288aa2, 0x711cc77a, 0x287a00c7, 0x222f8aa2, 0x00000008, 0x00000800, 0x00080000
    , 0x5208c252, 0x820000c5, 0x14885014, 0x2104a421, 0x010100a0, 0x00400008, 0x00000050, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00001800, 0x00000000, 0x00000000
    , 0x00000400, 0x00000000, 0x799ee779, 0xc7719ce7, 0x1cc7711c, 0x8aa2288a, 0x0882222a, 0x08822020
    , 0x799ee779, 0xcff320e7, 0x0882203c, 0x08822008, 0x288aa222, 0x088220a2, 0x711cc771, 0xc7711cc7
    , 0x1886611c, 0x00000000, 0x00000080, 0x00000000, 0x512c8520, 0x85200040, 0x14852014, 0x001a4240
    , 0x42400080, 0x00424000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00800000, 0x00000000, 0x711c2772, 0xc77100c7
    , 0x2c84701c, 0x8aa2284a, 0x28caa228, 0x228788a2, 0x8aa2684a, 0x28aa9428, 0xa48488a2, 0x8aa2a8ea
    , 0x28aa8828, 0xa88488a2, 0x8aa2284b, 0x28aa9428, 0xa44489a2, 0x8aa2284a, 0x289aa228, 0x22278aa2
    , 0x711c2772, 0x287200c7, 0x1c248aa2, 0x00000000, 0x00080000, 0x00000000, 0x5208c202, 0x820000c5
    , 0x00805014, 0x2104a401, 0x010100a0, 0x00400008, 0x00000000, 0x00001800, 0x00000000, 0x00000000
    , 0x00000400, 0x00000000, 0x8aa2288a, 0xeffb9c2b, 0x1cc771be, 0x8aa2288a, 0x0882222a, 0x08822020
    , 0x8aa2288a, 0x0882202a, 0x08822020, 0xfbbeeffb, 0xcff320ef, 0x0882203c, 0x8aa2288a, 0x0882202a
    , 0x08822020, 0x8aa2288a, 0x0882222a, 0x08822020, 0x711cc771, 0xeffb9cc7, 0x1cc771be, 0x00000000
    , 0x00000080, 0x00000000, 0x512c8520, 0x85200040, 0x14852014, 0x001a4240, 0x42400080, 0x00424000
    , 0x02000000, 0x00600000, 0x00000000, 0x02000000, 0x00100000, 0x00000000, 0x0300e003, 0x000080a2
    , 0x1ce11028, 0x02000000, 0x00008062, 0x22811014, 0x02008000, 0x00008022, 0x9047780a, 0x02008000
    , 0x00008c26, 0x08255014, 0x0200e003, 0x00008c2e, 0x08a33028, 0x40188730, 0xc701800e, 0x004d5100
    , 0x20048248, 0x8000800e, 0x08024100, 0x10080148, 0x82008007, 0x00044100, 0x00040530, 0x85010000
    , 0x0002c300, 0x00180200, 0x82000000, 0x000c4100, 0x00000000, 0x00000000, 0x00000000, 0x00000200
    , 0x00000000, 0x00000000, 0xa82c8700, 0xe0011c82, 0x8007000a, 0x50928a00, 0x10020282, 0x40080814
    , 0x8b108a00, 0x50020ce2, 0x400a0828, 0x50b88a00, 0x90021280, 0x40caf914, 0xab108700, 0x570212e2
    , 0x400b000a, 0x01120200, 0x10020c42, 0x40080000, 0x020c8000, 0xe3011022, 0x80070000, 0x00000000
    , 0x05500e00, 0x3e000000, 0x00000000, 0x03000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00002080, 0x00020000, 0x00000000, 0x00002080, 0x00010000, 0x00002104, 0x193ce8f1, 0x8f8814a2
    , 0x00802088, 0x2202288a, 0x44512a65, 0x00802008, 0x221c288a, 0x2222aa28, 0x00892008, 0x22a02c8a
    , 0x2152a228, 0x804a2010, 0xfa1eebf1, 0x2f8aa228, 0x80842088, 0x20000000, 0x00000000, 0x00802008
    , 0x20000000, 0x00000000, 0x00802008, 0x00000000, 0x00000000, 0x00002104, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x03001c00, 0x00000000, 0x00000000
    , 0x04000200, 0x00000080, 0x791cef01, 0xc0891ec4, 0x9ca872a2, 0x8aa22802, 0x80882204, 0xa2a822a4
    , 0x8ba0e801, 0x808822c4, 0xa2a822b8, 0x8aa22800, 0x8088222e, 0xa2ac22a4, 0x791ccf01, 0x81f11cc4
    , 0x1c4b23a2, 0x08000810, 0x00808004, 0x00002020, 0x08000820, 0x80800003, 0x000060a0, 0x00000040
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x3e000000, 0x00000000, 0x00000000, 0x00c0011c, 0x219ca881, 0x8f8814c2
    , 0x00400890, 0x22224982, 0x88882a25, 0x00401010, 0x2202aa82, 0x84502a25, 0x00401010, 0x221c2ff2
    , 0x8220a228, 0x00402010, 0x22a0288a, 0x4151a228, 0x00404010, 0x22a2288a, 0x208aa228, 0x80484090
    , 0xfa1ccff1, 0x2f8aa228, 0x00458090, 0x00000000, 0x00000000, 0x00c2011c, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0xf31c2f72, 0xc6891ce8, 0x9c28fa22, 0x4aa22482, 0x89882208, 0xa2288224
    , 0x4aa024ba, 0x81882608, 0xa2298228, 0x4b20e7ab, 0x81f820cf, 0xa22a8230, 0x4aa024ba, 0x81882008
    , 0xa2ac8228, 0x4aa2248a, 0x81882208, 0xa2688324, 0xf31ccf71, 0xc3899cef, 0x9c2882a2, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000030, 0x119ccf31, 0x867108c7
    , 0x08000018, 0x12228448, 0x46888828, 0x00041018, 0xf8028248, 0x20888828, 0x08e22300, 0x900c8148
    , 0xe671042f, 0x08014018, 0x53848048, 0x268a04c8, 0x04e22318, 0x32828849, 0x208a0204, 0x22041000
    , 0x133e8730, 0xc0713ee3, 0x1c000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x20000000
    , 0x00110000, 0x0000c000, 0x72148000, 0x82208066, 0x20066000, 0xaa3e0000, 0x8a200069, 0x10066088
    , 0x29148000, 0x4740800a, 0x10000008, 0x70148000, 0x42400084, 0x08e0033e, 0xa03e8000, 0x4740004a
    , 0x04000008, 0xab148500, 0x8a20082a, 0x04000088, 0x73008500, 0x82200824, 0x02000000, 0x20000500
    , 0x00110800, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0xfc000000, 0x80200082, 0x00000000, 0x00000000, 0x80200082, 0x00000000, 0x00000000, 0x8f200082
    , 0x000b51be, 0x003f0000, 0x80200082, 0x80045100, 0x00000000, 0x81200082, 0x00e453b0, 0x00c00f00
    , 0x86fc3ffe, 0x0c8e500c, 0x00000000, 0x88000882, 0x0ce4fb02, 0x00000000, 0x86000882, 0x8044000c
    , 0x0000f003, 0x81000882, 0x004300b0, 0x00000000, 0x80000882, 0x00000000, 0x00000000, 0x80000882
    , 0x00000000, 0x000000fc, 0x80000882, 0x00000000, 0x00400500, 0x00000000, 0x08002000, 0x00800a00
    , 0x00000000, 0x08002000, 0x204405a8, 0x00f800a2, 0x08002000, 0x20848a00, 0x000000a3, 0x08002000
    , 0x3044c589, 0x002000c2, 0x08002000, 0xa084ea03, 0x002080a3, 0xff03e038, 0xb96ec589, 0x00f800c0
    , 0x08020008, 0xc2a88a00, 0x00200c0e, 0x08020008, 0x827805a8, 0x00201208, 0x08020008, 0xe2a80a00
    , 0x00001208, 0x08020008, 0x01680500, 0x00000c88, 0x08020008, 0x00800a00, 0x00000000, 0x08020008
    ]

