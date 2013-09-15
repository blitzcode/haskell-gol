
{-# LANGUAGE BangPatterns, ForeignFunctionInterface, FlexibleContexts, QuasiQuotes #-}

module Grid (Grid
            , stepGrid
            , stepGridAsync
            , stepGridCpp
            , stepGridRepaNOP
            , stepGridRepaTraverse
            , stepGridRepaStaticStencil
            , stepGridRepaConvolve
            , stepGridRepaPartitioned) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Array.Repa as R
import           Data.Array.Repa (Z(..), (:.) (..))
import qualified Data.Array.Repa.Unsafe as RU (unsafeTraverse)
import qualified Data.Array.Repa.Stencil as RS (Boundary(BoundConst))
import           Data.Array.Repa.Stencil.Dim2 -- Need to import this unqualified because of TH
import qualified Data.Array.Repa.Algorithms.Convolve as RC (convolveOutP)
import qualified Data.Array.Repa.Repr.Partitioned as RP (Range(..))
import           Data.Array.Repa.Repr.Undefined   (X)
import           Data.Array.Repa.Repr.Delayed     (D)
import           Data.Array.Repa.Repr.Partitioned (P)
import           Data.Array.Repa.Repr.HintSmall   (S)
import Data.Word (Word8)
import Data.Bits ((.|.), (.&.))
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity(..), runIdentity)

type Grid = VU.Vector Bool

-- TODO: Optimize more by using unboxed types, strictness, inlining etc.

-- Life rules
{-# INLINE alive #-}
alive :: Grid -> Int -> Int -> Int -> Int -> Bool -> Bool
alive !g !w !h !x !y !check = (nb == 3) || (cell && nb == 2)
    where {-# INLINE idx #-}
          idx = x + y * w
          {-# INLINE cell #-}
          cell = g `VU.unsafeIndex` idx
          {-# INLINE foldstep #-}
          foldstep i a = a + if (g `VU.unsafeIndex` i) then 1 else 0
          nb = foldr foldstep 0 nbList :: Int
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

{-# INLINE repaAlive #-}
repaAlive :: Int -> Int -> Bool -> (R.DIM2 -> Bool) -> R.DIM2 -> Bool
repaAlive !w !h !check !lu !idx =
    let (Z :. y :. x) = idx
        {-# INLINE cell #-}
        cell = lu idx
        {-# INLINE foldstep #-}
        foldstep i a = a + (if lu i then 1 else 0)
        nb = foldr foldstep 0 nbList :: Int
        nbList | not check = nbListUnwrapped
               | otherwise = map (\(Z :. y' :. x') -> (Z :. (y' `mod` h) :. (x' `mod` w))) nbListUnwrapped
        -- Having any kind of 1D indexing for the no-wrap case didn't help
        nbListUnwrapped = [ (Z :. y     :. x + 1), (Z :. y + 1 :. x    )
                          , (Z :. y     :. x - 1), (Z :. y - 1 :. x    )
                          , (Z :. y + 1 :. x + 1), (Z :. y - 1 :. x - 1)
                          , (Z :. y - 1 :. x + 1), (Z :. y + 1 :. x - 1)
                          ]
     in (nb == 3) || (cell && nb == 2)

stepGrid :: Int -> Int -> Grid -> Grid
stepGrid !w !h !g = VU.create $ do -- ST monad (need locally mutable vectors)
    arr <- VUM.new $ w * h
    mapM_ (\(x, y, check) -> VUM.unsafeWrite arr (x + y * w) $ alive g w h x y check) $
        -- Borders, make torus array
        [(x, y, True ) | y <- [0, h - 1], x <- [0..w - 1]] ++
        [(x, y, True ) | x <- [0, w - 1], y <- [0..h - 1]] ++
        -- Inner part of the grid, no need for bounds checking
        [(x, y, False) | y <- [1..h - 2], x <- [1..w - 2]]
    return arr

stepGridAsync :: Int -> Int -> Grid -> IO Grid
stepGridAsync !w !h !g = do
    arr <- VUM.new $ w * h
    -- Inner part of the grid, no need for bounds checking, run in parallel on
    -- vertically partitioned segments
    let makeSegments nseg low high = map (\i -> (low + i * segl, low + (i + 1) * segl - 1)) [0..nseg - 2]
                                     ++ [(low + (nseg - 1) * segl, high)]
            where range  = high - low
                  segl = (range `div` nseg)
    nsegments <- (* 4) <$> getNumCapabilities -- Tweak Me: Use more segments than cores
    as <- mapM (\(h0, h1) -> async $
        mapM_ (\(x, y) -> VUM.unsafeWrite arr (x + y * w) $ alive g w h x y False)
        [(x, y) | y <- [h0..h1], x <- [1..w - 2]]) $ makeSegments nsegments 1 (h - 2)
    -- Borders, make torus array, run in parallel
    ab <- mapM (async . mapM_ (\(x, y) -> VUM.unsafeWrite arr (x + y * w) $ alive g w h x y True))
        [ [(x, y) | y <- [0, h - 1], x <- [0..w - 1]]
        , [(x, y) | x <- [0, w - 1], y <- [0..h - 1]]
        ]
    mapM_ wait $ as ++ ab
    VU.unsafeFreeze arr

stepGridRepaNOP,
    stepGridRepaTraverse,
    stepGridRepaStaticStencil,
    stepGridRepaConvolve,
    stepGridRepaPartitioned
    :: Int -> Int -> Grid -> Grid

-- Just to test / profile conversion code
stepGridRepaNOP =
    let {-# INLINE nop #-}
        nop _ _ g = return g
    in  repaConvertStencil nop

stepGridRepaTraverse h w grid =
    let {-# INLINE trav #-}
        trav g = RU.unsafeTraverse g id $ repaAlive w h True :: R.Array R.D R.DIM2 Bool
    in    R.toUnboxed
        . runIdentity
        . R.computeP
        . trav
        . R.fromUnboxed (Z :. h :. w)
        $ grid

-- Helper wrapping a parallel computation on Repa Word8 arrays inside our normal
-- step function. The conversion overhead seems reasonably low, but we could add
-- an nsteps parameter like for the C++ code
{-# INLINE repaConvertStencil #-}
repaConvertStencil :: R.Source r Word8 =>
                      (Int -> Int -> R.Array R.D R.DIM2 Word8 -> Identity (R.Array r R.DIM2 Word8)) ->
                       Int -> Int -> Grid                     -> Grid
repaConvertStencil f w h g =
    let srcB  = R.fromUnboxed (Z :. h :. w) g         :: R.Array R.U R.DIM2 Bool
        srcW8 = R.map (\x -> if x then 1 else 0) srcB :: R.Array R.D R.DIM2 Word8
        dstW8 = f w h srcW8
    in  runIdentity $ fmap R.toUnboxed . R.computeP . R.map (\x -> x /= (0 :: Word8)) =<< dstW8

-- Stencil version idea from http://www.tapdancinggoats.com/haskell-life-repa.htm

{-# INLINE stencilStep #-}
stencilStep :: Word8 -> Word8
stencilStep x | (x .&. 15) == 3 = 1 -- Three neighbors?
              | x == (16 .|. 2) = 1 -- Alive and 2 neighbors?
              | otherwise = 0

#ifndef NO_TH -- TH interferes with profiling, disable this code for such builds
stepGridRepaStaticStencil =
    let {-# INLINE applyStencil #-}
        applyStencil _ _ g = return $ R.map stencilStep .
                             -- Can't implement torus array, border cells always die
                             mapStencil2 (RS.BoundConst 32)
                             [stencil2| 1  1  1
                                        1 16  1
                                        1  1  1 |] -- TODO: Maybe put all TH code into a separate module?
                             $ g
    in  repaConvertStencil applyStencil
#else
stepGridRepaStaticStencil = stepGridRepaNOP
#endif -- NO_TH

stepGridRepaConvolve =
    let {-# INLINE applyStencil #-}
        applyStencil _ _ g = do
            let stencil = R.fromListUnboxed (Z :. 3 :. 3)
                          [ 1,  1,  1
                          , 1, 16,  1
                          , 1,  1,  1 ] :: R.Array R.U R.DIM2 Word8
            srcW8Computed <- R.computeP g
            dstW8Conv <- RC.convolveOutP (\lu (Z :. h :. w) (Z :. y :. x) ->
                                           lu (Z :. (y `mod` h) :. (x `mod` w))) stencil srcW8Computed
            return $ R.map stencilStep dstW8Conv
    in  repaConvertStencil applyStencil

type PD5 = P D (P (S D) (P (S D) (P (S D) (P (S D) X))))
stepGridRepaPartitioned w h grid =
    let partition !arr =
            -- We actually duplicated the array partitioning code from makeBordered2 here. Oddly
            -- enough, Repa's own mapStencil2 seems to do the same instead of using that function
            let sh = R.extent arr
                (inX    , inY    , inW    , inH    ) = (1    , 1    , w - 2, h - 2)
                (leftX  , leftY  , leftW  , leftH  ) = (0    , 0    , 1    , h    )
                (rightX , rightY , rightW , rightH ) = (w - 1, 0    , 1    , h    )
                (topX   , topY   , topW   , topH   ) = (0    , 0    , w    , 1    )
                (bottomX, bottomY, bottomW, bottomH) = (0    , h - 1, w    , 1    )
                {-# INLINE inInternal #-}
                inInternal (Z :. y :. x) = x >= inX && x < (inX + inW) &&
		                           y >= inY && y < (inY + inH)
                {-# INLINE inBorder #-}
	        inBorder = not . inInternal
                {-# INLINE arrI #-}
                arrI =            R.fromFunction sh $ repaAlive w h False (R.unsafeIndex arr)
                {-# INLINE arrB #-}
                arrB = R.ASmall . R.fromFunction sh $ repaAlive w h True  (R.unsafeIndex arr)
            in    R.APart sh (RP.Range (Z :.     inY :.     inX) (Z :.     inH :.     inW) inInternal) arrI
                $ R.APart sh (RP.Range (Z :.   leftY :.   leftX) (Z :.   leftH :.   leftW) inBorder  ) arrB
                $ R.APart sh (RP.Range (Z :.  rightY :.  rightX) (Z :.  rightH :.  rightW) inBorder  ) arrB
                $ R.APart sh (RP.Range (Z :.    topY :.    topX) (Z :.    topH :.    topW) inBorder  ) arrB
                $ R.APart sh (RP.Range (Z :. bottomY :. bottomX) (Z :. bottomH :. bottomW) inBorder  ) arrB
                $ R.AUndefined sh
                :: R.Array PD5 R.DIM2 Bool
    in    R.toUnboxed
        . runIdentity
        . R.computeP
        . R.delay
        . partition
        . R.fromUnboxed (Z :. h :. w)
        $ grid

-- TODO: Consider writing a version based on Data.Array.Repa.Eval.fillBlock2P

stepGridCpp :: Int -> Int -> Int -> Int -> Grid -> IO Grid
stepGridCpp nsteps nthread w h g = do
    -- Slow conversion code to get from unboxed Bool arrays to storable &
    -- mutable Word8 arrays and back. Call with more steps to compensate. Number
    -- of steps must be odd (see C++ code)
    srcVS <- VSM.new $ w * h
    let toChar = (\x -> if x then 1 else 0) :: Bool -> Word8
    mapM_ (\i -> VSM.unsafeWrite srcVS i $ toChar $ VU.unsafeIndex g i) [0..(w * h) - 1]
    dstVS <- VSM.new $ w * h
    VSM.unsafeWith srcVS $ \src ->
        VSM.unsafeWith dstVS $ \dst ->
            step_grid_c
            (fromIntegral nthread) (fromIntegral nsteps) (fromIntegral w) (fromIntegral h) src dst
    out <- VS.unsafeFreeze dstVS
    return $ VU.convert $ VS.map (/= 0) out

foreign import ccall unsafe "gol.h step_grid" step_grid_c
    :: CInt -> CInt -> CInt -> CInt -> Ptr Word8 -> Ptr Word8 -> IO ()

