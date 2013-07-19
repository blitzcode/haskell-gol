
module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Array.Unboxed
import Data.Array.Storable as SA
import Data.IORef
import Data.Word (Word32)
import Control.Applicative ((<$>))
import Numeric (showFFloat)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Random

type Grid = UArray (Int, Int) Bool

gridWidth, gridHeight :: Int
gridWidth  = 256
gridHeight = 256

main :: IO ()
main = do
    let grid = centerFromASCIIHelper acorn
    refGrid <- newIORef grid
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= growSize 0 15 (sizeFromGrid grid)
    (_, _) <- getArgsAndInitialize
    _ <- createWindow "Game of Life"
    reshapeCallback $= (Just $ reshape refGrid)
    refGen <- newIORef (0 :: Int)
    refTick <- newIORef (0 :: Int)
    displayCallback $= (display refGrid refGen refTick)
    idleCallback $= (Just $ idle refGrid)
    keyboardMouseCallback $= (Just $ keyboardMouse refGrid refGen)
    mainLoop

sizeFromGrid :: Grid -> Size
sizeFromGrid g = case bounds g of
    ((x0, y0), (x1, y1)) -> Size (fromIntegral $ x1 - x0 + 1) (fromIntegral $ y1 - y0 + 1)
growSize :: Int -> Int -> Size -> Size
growSize x y (Size w h) = Size (w + fromIntegral x) (h + fromIntegral y)

makeRandomGridIO :: Int -> Int -> IO Grid
makeRandomGridIO w h = do 
    gen <- newStdGen
    return $ makeRandomGrid w h gen
makeRandomGrid :: RandomGen g => Int -> Int -> g -> Grid
makeRandomGrid w h g = listArray ((0, 0), (w - 1, h - 1)) rndList
    where rndList = map ((<) 0.75) $ (randoms g :: [Float])

-- TODO: Only works with unboxed arrays (undefined outside of ASCII area)
gridFromASCII :: Int -> Int -> Int -> Int -> [String] -> Grid
gridFromASCII w h xoffs yoffs str = arr . zip idx . asc $ str
    where idx = [(x + xoffs, y + yoffs) | y <- [0..(length str) - 1], x <- [0..(length . head $ str) - 1]]
          asc = concatMap (map fromASCII)
              where fromASCII x = case x of { 'O' -> True; _ -> False }
          arr = array ((0, 0), (w - 1, h - 1))
centerFromASCIIHelper :: [String] -> Grid
centerFromASCIIHelper = gridFromASCII gridWidth gridHeight (gridWidth `quot` 2) (gridHeight `quot` 2)

acorn :: [String]
acorn =
    [
        ".O.....",
        "...O...",
        "OO..OOO"
    ]

gun :: [String]
gun =
    [
        "........................O...........",
        "......................O.O...........",
        "............OO......OO............OO",
        "...........O...O....OO............OO",
        "OO........O.....O...OO..............",
        "OO........O...O.OO....O.O...........",
        "..........O.....O.......O...........",
        "...........O...O....................",
        "............OO......................"
    ]

stepGrid :: Grid -> Grid
stepGrid g = listArray b $ map alive (indices g)
    where b@((x0, y0), (x1, y1)) = bounds g
          alive i@(ix, iy) = (cell && (nb == 2 || nb == 3)) || (not cell && nb == 3)
              where cell = g ! i
                    nb = countBool . map ({-# SCC "lookup" #-} (g !)) $ nbList
                    countBool = length . filter id
                    nbList = [wrapGrid (ix + x, iy + y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]
                        -- TODO: Assumes zero-based index
                        where wrapGrid (x, y) = (x `mod` (x1 - x0), y `mod` (y1 - y0)) 

reshape :: IORef Grid -> Size -> IO ()
reshape grid _ = do
    (windowSize $=) =<< growSize 0 15 . sizeFromGrid <$> readIORef grid
    postRedisplay Nothing

keyboardMouse :: IORef Grid -> IORef Int -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ (Char '\27' {- Escape -}) Down _ _ = exitWith ExitSuccess
keyboardMouse grid gen (Char 'r') Down _ _ = do
    writeIORef grid =<< (makeRandomGridIO gridWidth gridHeight)
    writeIORef gen 0
keyboardMouse grid gen (Char 'a') Down _ _ = do
    writeIORef grid $ centerFromASCIIHelper acorn
    writeIORef gen 0
keyboardMouse grid gen (Char 'g') Down _ _ = do
    writeIORef grid $ centerFromASCIIHelper gun
    writeIORef gen 0
keyboardMouse _ _ _ _ _ _ = return ()

idle :: IORef Grid -> IO ()
idle grid = do
    modifyIORef grid stepGrid
    postRedisplay Nothing

display :: IORef Grid -> IORef Int -> IORef Int -> IO ()
display grid gen tick = do
    clear [ColorBuffer]
    gridBounds <- bounds <$> readIORef grid
    gridSize <- sizeFromGrid <$> readIORef grid
    let toPixels = map (\x -> case x of { True -> 0x00FFFFFF; False -> 0x00000000 })
    gridElements <- toPixels . elems <$> readIORef grid
    arr <- newListArray gridBounds gridElements :: IO (SA.StorableArray (Int, Int) Word32)
    windowPos (Vertex2 (0 :: GLint) 0)
    withStorableArray arr
        (\ptr -> drawPixels gridSize (PixelData RGBA UnsignedByte ptr))
    displayStats gen tick gridSize
    swapBuffers

displayStats :: IORef Int -> IORef Int -> Size -> IO ()
displayStats gen tick gridSize = do
    windowPos (Vertex2 3 (case gridSize of Size _ h -> h + 3 :: GLint))
    color (Color3 (0.0 :: GLfloat) 1.0 0.0)
    curGen <- readIORef gen
    modifyIORef gen (+ 1)
    lastTick <- readIORef tick
    curTick <- get elapsedTime
    writeIORef tick curTick
    renderString Fixed8By13 $
        show curGen ++ "G | " ++
        showFFloat (Just 2) (1000.0 / fromIntegral (curTick - lastTick) :: Float) "" ++ "FPS | " ++
        "Press R G A"

