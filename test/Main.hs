{- Test distribution by comparing output with theoretical results
 -
 - Kolmogorov–Smirnov
 -
 -}

import           Data.Maybe (fromMaybe)

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Algorithms.Intro (sort)
import           Statistics.Types
import           Statistics.Distribution
import           Statistics.Distribution.Gamma
import           Statistics.Distribution.Random.Gamma

import           Statistics.Test.KolmogorovSmirnov

import qualified System.Random.MWC as R

import           Plot

-- number of random draws from generator
size :: Int
size = 5000

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
    --  benchmark times
    
    --  calc K-S test statistic
    let gd = gammaDistr shape (1/rate)
    let (d, p) = kolmogorovSmirnov gd v

    --  generate plot(s)
    let vs = V.modify sort v
    let cm = V.map (cumulative gd) vs
    plot cm d p "Test" "test.svg"

    putStrLn $ "Done "++ (show $ V.length vs)

