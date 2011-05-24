module Statistics.Test.KolmogorovSmirnov (
    kolmogorovSmirnov
) where

import           Statistics.Types
import           Statistics.Math (factorial)
import           Statistics.Distribution
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Algorithms.Intro (sort)
import qualified Data.Array.Repa as R
import           Data.Array.Repa.Index
import           Data.Array.Repa.Algorithms.Matrix

-- URL: http://www.jstatsoft.org/v08/i18/.

type DStatistic = Double
type PValue = Double

-- | One-sample Kolmogorov-Smirnov test
kolmogorovSmirnov :: (ContDistr d) => d -> Sample -> (DStatistic, PValue)
kolmogorovSmirnov dist sample = 
    let n = U.length sample
        diffs = differences dist sample
        d = U.maximum $ diffs U.++ (U.map (\de -> de - 1/(fromIntegral n)) diffs)
        p = 1 - k n d
    in  (d, p)

    where 
        differences :: (ContDistr d) => d -> Sample -> Sample
        differences d s = 
            let sorts = U.modify sort
                cumms = U.map (cumulative d)
                diffs = U.imap (\i e -> e - (fromIntegral i)/(fromIntegral n))
                n     = U.length s
            in diffs $ cumms $ sorts s


k :: Int -> Double -> Double
k size diff =
    let s =  (fromIntegral size) * diff * diff
    in if s > 7.24 || (s > 3.76 && size > 99)
        then kshort (fromIntegral size)  s
        else klong size diff
    where 
      kshort n s = 1 - 2 *(exp (-(2.000071 + 0.331 / (sqrt n) + 1.409/n) * s))
      klong n d =
        let t = truncate $ (fromIntegral n) * d 
            m = 2 * t + 1
            h = (fromIntegral (t+1)) - (fromIntegral n) * d
            
            -- Initialise H
            initH :: Int -> [Double]
            initH w = 
                do 
                    i <- [1 .. w]
                    j <- [1 .. w]
                    return $ elm j i
                    where 
                        elm i j 
                            | i - j + 1 < 0     = 0.0
                            | i < w && j > 1    = 1.0 / factorial (i - j + 1)
                            | j == 1 && i < w   = (1-h^i)/factorial i
                            | i == w && j > 1   = (1-h^(w-(j-1)))/factorial (w-(j-1))
                            | i == w && j == 1  = (1-2*h^w + (max 0 (2*h-1))^w)/factorial w
                            | otherwise         = 0.0
            mH = R.fromList (Z :. m :. m) (initH m)

            -- take n-th power of H 
            mHn = foldr1 multiplyMM (replicate n mH)

        in  ((factorial n) * (mHn R.! (Z :. t :. t))) / ((fromIntegral n)^n)

