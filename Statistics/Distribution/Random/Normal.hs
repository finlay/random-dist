{-# LANGUAGE FlexibleContexts #-}
module Statistics.Distribution.Random.Normal (normal) where

import Control.Monad
import Data.Bits
import Data.Word
import qualified Data.Vector.Unboxed as I
import Random.CRI
import Statistics.Distribution.Random.Uniform

-- Copied from mwc-random.

data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Generate a normally distributed random variate.
--
-- The implementation uses Doornik's modified ziggurat algorithm.
-- Compared to the ziggurat algorithm usually used, this is slower,
-- but generates more independent variates that pass stringent tests
-- of randomness.
normal :: (PrimSource m g Double, PrimSource m g Word32) => g m -> m Double
normal gen = loop
  where
    loop = do
      u  <- (subtract 1 . (*2)) `liftM` uniform gen
      ri <- uniform gen
      let i  = fromIntegral ((ri :: Word32) .&. 127)
          bi = I.unsafeIndex blocks i
          bj = I.unsafeIndex blocks (i+1)
      if abs u < I.unsafeIndex ratios i
        then return $! u * bi
        else if i == 0
        then normalTail (u < 0)
        else do
          let x  = u * bi
              xx = x * x
              d  = exp (-0.5 * (bi * bi - xx))
              e  = exp (-0.5 * (bj * bj - xx))
          c <- uniform gen
          if e + c * (d - e) < 1
            then return x
            else loop
    blocks = let f = exp (-0.5 * r * r)
             in (`I.snoc` 0) . I.cons (v/f) . I.cons r .
                I.unfoldrN 126 go $! T r f
      where
        go (T b g)   = let !u = T h (exp (-0.5 * h * h))
                           h  = sqrt (-2 * log (v / b + g))
                       in Just (h, u)
        v            = 9.91256303526217e-3
    {-# NOINLINE blocks #-}
    r                = 3.442619855899
    ratios           = I.zipWith (/) (I.tail blocks) blocks
    {-# NOINLINE ratios #-}
    normalTail neg  = tailing
      where tailing  = do
              x <- ((/r) . log) `liftM` uniform gen
              y <- log          `liftM` uniform gen
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - r else r - x
{-# INLINE normal #-}


