{-# LANGUAGE ScopedTypeVariables #-}
module Statistics.Sampler.Slice (
  slice,
  adaptWidth
) where

import qualified System.Random.MWC as R
import qualified Statistics.Distribution.Random.Exponential as E
import           Control.Monad (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)

import           Prelude hiding (max)

-- | slice sampling as described in
-- Radford M. Neal, "Slice sampling",
-- Ann. Statist. Volume 31, Number 3 (2003), 705-767.
-- http://projecteuclid.org/euclid.aos/1056562461

slice :: (PrimMonad m) =>
         (Double -> Double)  -- ^ x -> log(f x) where f is probibility function
      -> Double              -- ^ width of step out size (approximate scale parameter)
      -> Int                 -- ^ maximum number of step outs
      -> Double              -- ^ current value
      -> R.Gen (PrimState m) -- ^ a random number generator
      -> m Double            -- ^ sample value
slice g width max x0 rng =
  do
    let g0 = g x0

    when (isInfinite g0) $
        error "Infinite value found in slice sampler"

    -- 1. define slice
    e <- E.exponential rng
    let z = g0 - e

    -- 2. find interval
    u <- R.uniform rng
    let l = x0 - width * u
        r = l + width

    v :: Double <- R.uniform rng
    let j = floor (fromIntegral max * v)
        k = (max - 1) - j

    let left = calc_left j l
        calc_left 0 l' = l'
        calc_left n l' =
            if z >= g l'
                then l'
                else calc_left (n-1) (l' - width)


    let right = calc_right k r
        calc_right 0 r' = r'
        calc_right n r' =
            if z >= g r'
                then r'
                else calc_right (n-1) (r' + width)

    -- 3. loop until accept (guaranteed)
    let sample left' right' =
          do
            u' <- R.uniform rng
            let x = left' + u' * (right' - left')
            if z < g x
              then return x -- accept
              else
                if x < x0
                  then sample x     right'
                  else sample left' x

    sample left right

adaptWidth :: Double -> Double -> Double
adaptWidth sumdiff iter = 2 * sumdiff / iter / (iter - 1)
