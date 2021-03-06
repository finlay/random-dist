{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{- Test distribution by comparing output with theoretical results
 -
 - Kolmogorov–Smirnov
 -
 -}

import           System.Environment (getArgs)
import           Data.List (intersperse)
import           Text.Printf

import           Control.Monad ( (>=>) )

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Algorithms.Intro (sort)
import qualified Random.CRI.MWC as R
import           Statistics.Distribution
import           Statistics.Distribution.Gamma
import           Numeric.MathFunctions.Constants
import           Statistics.Test.KolmogorovSmirnov

import           Statistics.Distribution.Random.Gamma
import           Statistics.Sampler.Slice

import           Plot

--import Test.Framework

main :: IO()
main = mainSlice
--    args <- filter (/= "--tests") `fmap` getArgs
--    flip defaultMainWithArgs args
--        [ Statistics.Distribution.Random.Gamma.testSuite
--        ]

-- slice test
mainSlice :: IO ()
mainSlice = do

    args <- getArgs
    let size :: Int
        title :: String
        outfile :: String
        (size, sampler, gd, initial, title, outfile) =
          case args of
            [size', "gamma", shape', rate' ] -> 
                 (read size', slice slst (log . (density  gammad)), gammad, shape/rate, title', outfile')
                    where
                       shape :: Double = read shape'
                       rate  :: Double = read rate'
                       gammad  = gammaDistr shape (1/rate)
                       slst   = adaptOff (newSlicerState 0.0001 m_pos_inf 1)
                       title' = printf "shape = %f, rate = %f" shape rate
                       outfile' =  printf "slice-%f-%f.csv" shape rate 
                                                
            _ -> error "Usage: ./test size gamma shape rate"

    rng <- R.create

    --  get samples
    let sample res = do
        !(_, ns) <- sampler (head res) rng
        return (ns:res)
    samples <- foldr1 (>=>) (replicate (size-1) sample) [initial]
    let v = V.fromList samples


    -- print out samples
    let pv = "slice\n" ++ (concat $ intersperse "\n" $ map show $ V.toList v)
    writeFile outfile pv
    --  benchmark times

    --  calc K-S test statistic
    let d = kolmogorovSmirnovD gd v
    let p = kolmogorovSmirnovProbability (V.length v) d

    putStrLn $ printf "D = %f, p = %f" d p

    --  generate plot(s)
    let vs = V.modify sort v
    let cm = V.map (cumulative gd) vs

    _ <- plot cm title "test-slice.svg"

    _ <- plotDensity gd vs title "test-slice-density.svg"

    putStrLn $ "Done "++ (show $ V.length vs)


-- original test
mainGamma :: IO ()
mainGamma = do

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
    let d = kolmogorovSmirnovD gd v
    let p = kolmogorovSmirnovProbability (V.length v) d

    putStrLn $ printf "D = %f, p = %f" d p

    --  generate plot(s)
    let vs = V.modify sort v
    let cm = V.map (cumulative gd) vs
    let title = printf "D = %f, p-value = %f" d p

    _ <- plot cm title "test.svg"

    let titledensity = printf "shape = %f, rate = %f" shape rate
    _ <- plotDensity gd vs titledensity "test-density.svg"

    putStrLn $ "Done "++ (show $ V.length vs)

