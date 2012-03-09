{-# LANGUAGE FlexibleContexts #-}
module Statistics.Distribution.Random.Uniform (
    uniform
) where

import Random.CRI

uniform :: Source m g a => g m -> m a
uniform = grandom
