{-# LANGUAGE BangPatterns #-}
module Statistics.Distribution.Random.Poisson where

-- We are using the algorithm PD as described in
--
-- J.H. Ahrens and U. Dieter
-- Computer Generation of Poisson Deviates from Modified Normal Distributions
-- ACM Transactions of Mathematical Software, 8(2), 163--179.
-- 1982.

import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.List
import qualified Data.Vector as V
import qualified System.Random.MWC as R
import qualified Statistics.Distribution.Random.Exponential as E

-- The algorithm distinguishes between two major cases: mu >= 10 (called
-- Case A) and mu < 10 (called Case B).
--
-- Depending on this condition, a completely different algorithm
-- is chosen. Furthermore, the algorithm is optimized for using the
-- same mu repeatedly, albeit in a very imperative fashion. Here,
-- we implement the Poisson distribution simply in such a way that
-- partial parameterization to mu will save unnecessary recomputations.

poisson :: (PrimMonad m) => Double -> R.Gen (PrimState m) -> m Int
poisson mu
  | mu == 0   = const (return 0)
  | mu >= 10  = caseA mu
  | otherwise = caseB mu

-- TODO: Above, treat the case where mu < 0.

-- TODO: Perhaps we should return Integral rather than Int.

-- Factorial table.
fact :: V.Vector Double
fact = V.unfoldrN 10
         (\ (!i, !j) -> Just (fromIntegral j, (i + 1, i * j))) (1, 1)

-- Coefficients of the polynomial a.
as :: [Double]
as = [a7,a6,a5,a4,a3,a2,a1]

a0,a1,a2,a3,a4,a5,a6,a7 :: Double
a0 = -0.49999999
a1 =  0.33333328
a2 = -0.25000678
a3 =  0.20001178
a4 = -0.16612694
a5 =  0.14218783
a6 = -0.13847944
a7 =  0.12500596

horner :: [Double] -> Double -> Double
horner q r = foldl' (\ a b -> (a + b) * r) 0 q
{-# INLINE horner #-}

caseA :: (PrimMonad m) => Double -> R.Gen (PrimState m) -> m Int
caseA mu = go
  where
    -- Quantities depending only on mu, described at the beginning of
    -- Case A.
    s     = sqrt mu
    d     = 6 * mu * mu
    bL    = floor (mu - 1.1484) -- TODO: explain constant

    -- Quantities depending only on mu, described in Step P. Laziness
    -- will ensure that these aren't actually recomputed unless needed.
    omega = (1 / sqrt (2 * pi)) / s -- TODO: constant evaluated?
    b1    = (1 / 24) / mu
    b2    = (3 / 10) / (b1 * b1)
    c3    = (1 / 7) * b1 * b2
    c2    = b2 - 15 * c3
    c1    = b1 - 6 * b2 + 45 * c3
    cs    = [c3,c2,c1]
    c0    = 1 - b1 + 3 * b2 - 15 * c3
    c     = 0.1069 / mu -- TODO: explain constant

    go rng = stepN
      where

        -- Step N (Normal sample).
        stepN =
          do
            bT <- R.normal rng
            let bG = mu + s * bT
            if mu + s * bT >= 0
              then stepsIS (floor bG)
              else stepE  -- Step P in the paper; see above for P.

        -- Steps I (Immediate acceptance) and S (Squeeze acceptance).
        stepsIS bK
          | bK >= bL  = return bK
          | otherwise = do
            bU <- R.uniform rng
            if d * bU >= (mu - fromIntegral bK) ^ (3 :: Int)
              then return bK
              else stepsFQ bK bU -- Step P in paper ...

        -- Procedure F followed by Step Q (Quotient acceptance).
        stepsFQ bK bU = do
          let (px, py, fx, fy) = procF bK
          if fy * (1 - bU) <= py * exp (px - fx)
            then return bK
            else stepE

        -- Step E (Double exponential sample).
        stepE = do
          bE <- E.exponential rng
          bU <- liftM (\ u -> u + u - 1) (R.uniform rng)
          let bT = 1.8 + bE * signum bU
          if bT < -0.6744
            then stepE
            else stepsFH (floor (mu + s * bT)) bU bE

        -- Procedure F followed by Step H (Hat acceptance).
        stepsFH bK bU bE = do
          let (px, py, fx, fy) = procF bK
          if c * abs bU > py * exp (px + bE) - fy * exp (fx + bE)
            then stepE
            else return bK

        -- Procedure F.
        procF :: Int -> (Double, Double, Double, Double)
        procF bK = (px, py, fx, fy)
          where
            (!px, !py)
              | bK < 10   = (-mu, mu ^ bK / fact V.! bK)
              | otherwise = (if abs bV <= 0.25
                               then bKd * bV * bV * (horner as bV + a0) - delta
                               else bKd * log (1 + bV) - (mu - bKd) - delta,
                             (1 / sqrt (2 * pi)) / sqrt bKd)
            (!fx, !fy)    = (0.5 * bX * bX,
                             omega * (horner cs (bX * bX) + c0))
            bKd           = fromIntegral bK
            !bX           = (bKd - mu + 0.5) / s
            delta'        = 1 / (12 * bKd)
            delta         = delta - 4.8 * delta * delta * delta
            bV            = (mu - bKd) - bKd



-- We can rather simplify Case B w.r.t. the paper, because most
-- of this part is concerned with calculating array P on demand.
-- We can just achieve this by making use of laziness.

caseB :: (PrimMonad m) => Double -> R.Gen (PrimState m) -> m Int
caseB mu = go
  where
    -- Quantities depending only on mu.
    bM = max 1 (floor mu)
    p0 = exp (-mu)
    bP = V.unfoldrN 36 genP (0, p0, p0)

    -- Generation function for the array. Tricky deviation from the
    -- paper: We use q before changing it, but in turn, we also start
    -- at index 0.
    genP (!bK, !p, !q) = Just (q, (bK + 1, p * mu / bK, q + p))

    go rng = stepU
      where

        -- Step U (Uniform sample).
        stepU = do
          bU <- R.uniform rng
          if bU <= p0
            then return 0
            else stepT bU

        -- Step T (Comparison of bU with existing table).
        stepT bU = do
          let bJ | bU > 0.458 = bM
                 | otherwise  = 1
          case V.findIndex (>= bU) (V.drop bJ bP) of
            Just bK' -> return (bK' + bJ) -- adjust index
            Nothing  -> stepU -- repeat

