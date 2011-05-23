module Statistics.Test.KolmogorovSmirnov (
    kolmogorovSmirnov
) where

import           Statistics.Types
import           Statistics.Math (factorial)
import           Statistics.Distribution
import qualified Data.Vector.Unboxed as U (modify, map, imap)
import           Data.Vector.Algorithms.Intro (sort)
import qualified Data.Array.Repa as R
import           Data.Array.Repa.Index
import           Data.Array.Repa.Algorithms.Matrix

type DStatistic = Double
type PVAlue = Double

-- | One-sample Kolmogorov-Smirnov test
kolmogorovSmirnov :: (ContDistr d) => d -> Sample -> (DStatistic, PValue)
kolmogorovSmirnov dist sample = 
    let diffs = calc_diffs dist sample
    in  s 

-- | calc differences
calc_diffs :: (ContDistr d)  => d -> Sample -> Sample
calc_diffs dist sample = imap (\ i e -> e - i/n) $ map (cumulative dist) $ modify sort sample



k :: Int -> Double -> Double
k n d =
    let k = truncate $ n * d + 1
        m = 2 * k - 1
        h = k - n * d

        -- Initialise H
        initH :: Int -> [Double]
        initH d = 
            do 
                i <- [1 .. d]
                j <- [1 .. d]
                return $ elm j i
                where 
                    elm i j 
                        | i - j + 1 < 0     = 0.0
                        | i < d && j > 1    = 1.0 / factorial (i - j + 1)
                        | j == 1 && i < d   = (1-h^i)/factorial i
                        | i == d && j > 1   = (1-h^(d-(j-1)))/factorial (d-(j-1))
                        | i == d && j == 1  = (1-2*h^d + (max 0 (2*h-1))^d)/factorial d
                        | otherwise         = 0.0
        mH = R.fromList (Z :. m :. m) (initH m)
        mHn = foldr1 multiplyMM (replicate n mH)
    in  ((factorial n) * (mHn R.! (Z :. k :. k))) / ((fromIntegral n)^n)

--    -- Fix if out on the tail
--    for(i = 1; i <= n; i++) {
--        s = s * i / n;
--        if(s < 1e-140) {
-- 	   s *= 1e140;
-- 	   eQ -= 140;
--        }
--    }

