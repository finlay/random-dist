{-# LANGUAGE FlexibleContexts #-}
module Statistics.Distribution.Random.Uniform (
    uniform
) where

import Random.CRI

uniform :: PrimSource m g a => g m -> m a
uniform = prandom
