{- Test distribution by comparing output with theoretical results
 -
 - Kolmogorov–Smirnov
 -
 -}

import           Data.Maybe (fromMaybe)

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Algorithms.Intro (sort)
import           Statistics.Distribution
import           Statistics.Distribution.Gamma
import           Statistics.Distribution.Random.Gamma

import qualified System.Random.MWC as R

import           Plot

-- number of random draws from generator
size :: Int
size = 50000

type Samples = V.Vector Double

-- Assumes that the samples are sorted
empirical :: Samples -> Double -> Double
empirical v x =
    let s = fromMaybe (V.length v) (V.findIndex (x <=) v)
    in  fromIntegral s / fromIntegral (V.length v)


-- main test
main :: IO ()
main = do
    -- for each parameters
    let shape  = 2
    let rate   = 0.9
    rng <- R.create
    let gs = gamma shape (1/rate)
    --  get samples
    v <- V.replicateM size (gs rng)

    --     benchmark times
    let vs = V.modify sort v
    --  sort samples
    --  calc K-S test statistic

    let gd = gammaDistr shape (1/rate)
    let xmax = V.last vs
    let xs = [0,(xmax / 1000) .. xmax ]
    let tv = [(x, (cumulative gd x)) | x <- xs]
    let ev = [(x, (empirical  vs x)) | x <- xs]

    --  generate plot(s)
    plot tv ev "Test" "test.svg"

    putStrLn $ "Done "++ (show $ V.length vs)

