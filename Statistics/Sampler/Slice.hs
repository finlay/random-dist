{-# LANGUAGE ScopedTypeVariables #-}
module Statistics.Sampler.Slice (
  slice,
  newSlicerState,
  adaptOff,
  SlicerState()
) where

import qualified System.Random.MWC as R
import qualified Statistics.Distribution.Random.Exponential as E
import           Statistics.Constants (m_epsilon)
import           Control.Monad (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)

import           Prelude hiding (max)

-- | slice sampling as described in
-- Radford M. Neal, "Slice sampling",
-- Ann. Statist. Volume 31, Number 3 (2003), 705-767.
-- http://projecteuclid.org/euclid.aos/1056562461

data SlicerState  = SlicerState {
    lower    :: Double,             -- ^ lower bound of distribution
    upper    :: Double,             -- ^ upper bound of distribution
    width    :: Double,             -- ^ width of step out size (approximate scale parameter)
    steps    :: Int,                -- ^ maximum number of step outs
    adapt    :: Bool,               -- ^ adapt phase underway
    sumdiff  :: Double,             -- ^ store sumdiff for adaption phase
    iter     :: Int                 -- ^ number of iterations
    } deriving Show

newSlicerState :: Double    -- ^ lower bound
               -> Double    -- ^ upper bound
               -> Double    -- ^ initial width
               -> SlicerState
newSlicerState l u w = SlicerState {
                          lower   = l,
                          upper   = u,
                          width   = w,
                          steps   = 10,
                          adapt   = True,
                          iter    = 1,
                          sumdiff = 0.0
                        }

adaptOff :: SlicerState -> SlicerState
adaptOff st = st { adapt = False }

slice :: (PrimMonad m) =>
         SlicerState
      -> (Double -> Double)       -- ^ x -> log(f x) where f is proportional to probibility density
      -> Double                   -- ^ current value
      -> R.Gen (PrimState m)      -- ^ a random number generator
      -> m (SlicerState, Double)  -- ^ return slicer state and new sample value
slice st g x0 rng =
  do
    let g0 = g x0

    when (isInfinite g0) $
        error $ "Infinite value found in slice sampler: " ++ (show x0) ++ " -> " ++ (show g0)

    when (isNaN g0) $
        error $ "NaN found in slice sampler: " ++ (show x0) ++ " -> " ++ (show g0)

    -- 1. define slice
    e <- E.exponential rng
    let z = g0 - e

    -- 2. find interval
    u <- R.uniform rng
    let l = x0 - (width st) * u
        r = l + (width st)

    v :: Double <- R.uniform rng
    let j = floor (fromIntegral (steps st) * v)
        k = ((steps st) - 1) - j

    let left = calc_left j l
        calc_left n l'
            | l' < (lower st)  = lower st
            | n == 0           = l'
            | z >= g l'        = l'
            | otherwise        = calc_left  (n-1) (l' - (width st))

    let right = calc_right k r
        calc_right n r'
            | r' > (upper st)  = upper st
            | n == 0           = r'
            | z >= g r'        = r'
            | otherwise        = calc_right (n-1) (r' + (width st))

    -- 3. loop until accept (guaranteed)
    let sample left' right' =
          do
            u' <- R.uniform rng
            let x = left' + u' * (right' - left')
            if z - m_epsilon <= g x
              then return x -- accept
              else
                if x < x0
                  then sample x     right'
                  else sample left' x

    x1 <- sample left right

    let st' = if adapt st
                then adaptSlicer x0 x1 st
                else st

    return (st', x1)

adaptSlicer :: Double -> Double -> SlicerState -> SlicerState
adaptSlicer old new oldst = newst
  where
    iterf    = fromIntegral (iter oldst)
    sumdiff' = (sumdiff oldst) +  iterf * (abs (new - old))
    newst    = oldst {
                    sumdiff = sumdiff',
                    iter    = (iter oldst) + 1,
                    width   = if (iter oldst) > 50
                                 then (2 * sumdiff' / (iterf * (iterf - 1)))
                                 else (width oldst)
                    }

