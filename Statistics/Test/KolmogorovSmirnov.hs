module Statistics.Test.KolmogorovSmirnov (
    kolmogorovSmirnov
) where

import           Statistics.Types
import           Statistics.Distribution
import qualified Data.Vector.Unboxed as U (modify, map, imap)
import           Data.Vector.Algorithms.Intro (sort)

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
