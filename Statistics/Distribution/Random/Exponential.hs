module Statistics.Distribution.Random.Exponential (
    exponential
) where


import qualified System.Random.MWC   as R
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)

exponential :: PrimMonad m => R.Gen (PrimState m) -> m Double
exponential rng = liftM (negate . log) (R.uniform rng)
