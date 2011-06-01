module Statistics.Test.KolmogorovSmirnov (
    kolmogorovSmirnov,
    kolmogorovSmirnovExact
) where

import           Data.List (foldl')
import           Statistics.Types
import           Statistics.Math (factorial)
import           Statistics.Distribution
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Algorithms.Intro (sort)
import qualified Data.Array.Repa as R
import           Data.Array.Repa.Index
import           Data.Array.Repa.Algorithms.Matrix

kolmogorovSmirnovExact :: (ContDistr d) => d -> Sample -> (Double, Double)
kolmogorovSmirnovExact = kolmogorovSmirnov' True
kolmogorovSmirnov :: (ContDistr d) => d -> Sample -> (Double, Double)
kolmogorovSmirnov = kolmogorovSmirnov' False

-- | One-sample Kolmogorov-Smirnov test
kolmogorovSmirnov' :: (ContDistr d) => Bool -> d -> Sample -> (Double, Double)
kolmogorovSmirnov' exact dist sample = 
    let n = U.length sample
        diffs = differences dist sample
        d = U.maximum $ diffs U.++ (U.map (\de -> de - 1/(fromIntegral n)) diffs)
        s =  (fromIntegral n) * d * d
        ks = if not exact && (s > 7.24 || (s > 3.76 && n > 99))
                    then kshort (fromIntegral n)  s
                    else k n d
        p = 1 - ks
    in  (d, p)

    where 
        differences :: (ContDistr d) => d -> Sample -> Sample
        differences d s = 
            let sorts = U.modify sort
                cumms = U.map (cumulative d)
                diffs = U.imap (\i e -> e - (fromIntegral i)/(fromIntegral n))
                n     = U.length s
            in diffs $ cumms $ sorts s
        kshort :: Double -> Double -> Double
        kshort n s = 1 - 2 *(exp (-(2.000071 + 0.331 / (sqrt n) + 1.409/n) * s))


--  "Evaluating Kolmogorov's distribution".
--  George Marsaglia and Wai Wan Tsang and Jingbo Wang (2003),
--  Journal of Statistical Software, Volume 8, 2003, Issue 18.
--  http://www.jstatsoft.org/v08/i18/.
    
k :: Int -> Double -> Double
k n d =
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
        (eQ, mHn) = matrixpower n (0, mH)
                   where 
                    matrixpower :: Int -> (Int, R.Array DIM2 Double) -> (Int, R.Array DIM2 Double)
                    matrixpower 1 p  = p
                    matrixpower i (q, mat) =
                            let (q', mat') = matrixpower (i `div` 2) (q, mat)
                                mat2 = multiplyMM mat' mat'
                                q2 = q' * 2
                                matf = if i `mod` 2 == 1 then multiplyMM mat2 mat else mat2
                            in scale (q2, matf)
                    scale :: (Int, R.Array DIM2 Double) -> (Int, R.Array DIM2 Double)
                    scale (q, mat) = 
                            let sc = R.force . R.map (*1e-140)
                            in  if mat R.! (Z :. t :. t) > 1e140 then (q + 140, sc mat) else (q,  mat)

        s =  mHn R.! (Z :. t :. t)
        (eQ', s') = foldl' unscale (eQ, s)  [1 .. n]
                    where
                      unscale :: (Int, Double) -> Int -> (Int, Double)
                      unscale (qq, ss) i =
                        let ss' = ss * (fromIntegral i) / (fromIntegral n)
                        in if ss' < 1e-140
                            then (qq - 140, ss' * 1e140)
                            else (qq,       ss')

    in  s' * (10.0 ^ eQ')


