{-# LANGUAGE FlexibleContexts #-}
module Statistics.Distribution.Random.Exponential (
    exponential
) where

import Control.Monad
import Random.CRI
import Statistics.Distribution.Random.Uniform

exponential :: Source m g Double => g m -> m Double
exponential rng = liftM (negate . log) (uniform rng)
