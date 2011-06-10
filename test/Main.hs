{- Test distribution by comparing output with theoretical results
 -
 - Kolmogorov–Smirnov
 -
 -}

import           System.Environment (getArgs)
import           Data.List (intersperse)
import           Text.Printf

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Algorithms.Intro (sort)
import           Statistics.Distribution
import           Statistics.Distribution.Gamma
import           Statistics.Distribution.Random.Gamma

import           Statistics.Test.KolmogorovSmirnov

import qualified System.Random.MWC as R

import           Plot


-- main test
main :: IO ()
main = do

    args <- getArgs

    let size  :: Int
        shape :: Double
        rate  :: Double
        (size, shape, rate) =
          case args of
            [size', shape', rate'] -> (read size', read shape', read rate')
            _                      -> error "Usage: ./test size shape rate"

    rng <- R.create
    let gs = gamma shape (1/rate)

    --  get samples
    v <- V.replicateM size (gs rng)

    -- print out samples
    let pv = "gammas\n" ++ (concat $ intersperse "\n" $ map show $ V.toList v)
    writeFile (printf "gammas-%f-%f.csv" shape rate) pv
    --  benchmark times

    --  calc K-S test statistic
    let gd = gammaDistr shape (1/rate)
    let (d, p) = kolmogorovSmirnov gd v

    putStrLn $ printf "D = %f, p = %f" d p

    --  generate plot(s)
    let vs = V.modify sort v
    let cm = V.map (cumulative gd) vs
    let title = printf "D = %f, p-value = %f" d p

    plot cm title "test.svg"

    let titledensity = printf "shape = %f, rate = %f" shape rate
    plotDensity gd vs titledensity "test-density.svg"

    putStrLn $ "Done "++ (show $ V.length vs)

