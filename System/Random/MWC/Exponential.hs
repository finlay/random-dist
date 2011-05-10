module System.Random.MWC.Exponential (
    exponential
) where


import qualified System.Random.MWC   as R
import Control.Monad.Primitive (PrimMonad, PrimState)

exponential :: PrimMonad m => R.Gen (PrimState m) -> m Double
exponential rng = do 
    u <- R.uniform rng
    return $ - (log u)


