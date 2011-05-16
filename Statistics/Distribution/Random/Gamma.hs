{-# LANGUAGE DoAndIfThenElse #-}
module Statistics.Distribution.Random.Gamma (
   gamma
) where

{- Algorithm borrowed from rmath/rgamma.c
 -
 - [1] Shape parameter a >= 1.  Algorithm GD in:
 -
 -  Ahrens, J.H. and Dieter, U. (1982).
 -  Generating gamma variates by a modified
 -  rejection technique.
 -  Comm. ACM, 25, 47-54.
 -
 -
 - [2] Shape parameter 0 < a < 1. Algorithm GS in:
 -
 -  Ahrens, J.H. and Dieter, U. (1974).
 -  Computer methods for sampling from gamma, beta,
 -  poisson and binomial distributions.
 -  Computing, 12, 223-246.
 -}

import qualified System.Random.MWC as R
import qualified Statistics.Distribution.Random.Exponential as E
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.List ( foldl' )
import           Data.Number.LogFloat ( expm1 )

sqrt32, exp_m1 :: Double
sqrt32 = sqrt 32 -- 5.656854
{-# NOINLINE sqrt32 #-}
exp_m1 = exp (-1) -- 0.36787944117144232159 -- exp(-1) = 1/e
{-# NOINLINE exp_m1 #-}

q1, q2, q3, q4, q5, q6, q7 :: Double
q1 =  0.04166669
q2 =  0.02083148
q3 =  0.00801191
q4 =  0.00144121
q5 = -7.388e-5
q6 =  2.4511e-4
q7 =  2.424e-4
qs :: [Double]
qs = [q7, q6, q5, q4, q3, q2, q1]
{-# INLINE qs #-}

a1, a2, a3, a4, a5, a6, a7 :: Double
a1 =  0.3333333
a2 = -0.250003
a3 =  0.2000062
a4 = -0.1662921
a5 =  0.1423657
a6 = -0.1367177
a7 =  0.1233795
as :: [Double]
as = [a7, a6, a5, a4, a3, a2, a1]
{-# INLINE as #-}

horner :: [Double] -> Double -> Double
horner q r = foldl' (\ a b -> (a + b) * r) 0 q
{-# INLINE horner #-}

gamma :: (PrimMonad m) => Double -> Double -> R.Gen (PrimState m) -> m Double
gamma shape scale rng
    | shape <  1    =  gammaGS shape scale rng
    | shape == 0    =  return 0
    | otherwise     =  gammaGD shape scale rng

gammaGS :: (PrimMonad m) => Double -> Double -> R.Gen (PrimState m) -> m Double
gammaGS shape scale rng =
    let e  = 1 + exp_m1 * shape
        go = do
            ru <- R.uniform     rng
            re <- E.exponential rng
            let p                  = e * ru
                x      | p >= 1    = - log ((e - p) / shape)
                       | otherwise = exp (log p / shape)
                accept | p >= 1    = re >= (1 - shape) * log x
                       | otherwise = re >= x
            if accept then return (scale * x) else go
    in go

gammaGD :: (PrimMonad m) => Double -> Double -> R.Gen (PrimState m) -> m Double
gammaGD shape scale rng =

    -- Step 1: Calculations of s2, s, d
    let s2 = shape - 0.5
        s  = sqrt s2
        d  = sqrt32 - s * 12
        -- Step 4: Calculations of q0, b, si, c
        r  = 1 / shape
        q0 = horner qs r

        -- Approximation depending on size of parameter shape
        -- The constants in the expressions for b, si and c
        -- were established by numerical experiments
        (b, si, c)
            | shape <=  3.686 = (0.463 + s + 0.178 * s2, 1.235,            0.195 / s - 0.079 + 0.16 * s)
            | shape <= 13.022 = (1.654 + 0.0076 * s2,    1.68 / s + 0.275, 0.062 / s + 0.024)
            | otherwise       = (1.77,                   0.75,             0.1515 / s)

        -- Step 6: calculation of v and quotient q
        calc_q t
            | abs v <= 0.25   = q0 + 0.5 * t * t * horner as v
            | otherwise       = q0 - s * t + 0.25 * t * t + (s2 + s2) * log (1 + v)
            where v = t / (s + s)

        choose_t = do
            -- Step 8: e = standard exponential deviate
            --         u =  0,1 -uniform deviate
            --         t = (b,si)-double exponential (laplace) sample
            e  <- E.exponential rng
            u' <- R.uniform rng
            let uu = u' + u' - 1
            let tt = if uu < 0 then b - si * e else b + si * e
            -- Step  9:  rejection if t < tau(1) = -0.71874483771719
            if tt >= -0.71874483771719
            then do
                -- Step 10:     calculation of v and quotient q
                let qq = calc_q tt
                -- Step 11:     hat acceptance (h)
                -- (if q not positive go to step 8)
                if qq > 0  &&  c * abs uu <= expm1 qq * exp (e - 0.5 * tt * tt)
                then return tt
                -- if t is rejected sample again at step 8
                else choose_t
            else choose_t -- loop until matches

        -- Step 2: t = standard normal deviate,
        --         x = (s,1/2) -normal deviate.
        -- immediate acceptance (i)
    in do
        t <- R.normal rng
        let x = s + 0.5 * t
        let ret_val = scale * x * x
        if t >= 0
        then return ret_val
        else do

        -- Step 3: u = 0,1 - uniform sample. squeeze acceptance (s)
        u <- R.uniform rng
        if d * u <= t * t * t
        then return ret_val
        else do

        -- Step 5: no quotient test if x not positive
        -- Step 7: quotient acceptance (q)
        let q = calc_q t
        if x > 0 && log (1 - u) <= q
        then return ret_val
        else do
        ttt <- choose_t
        let xx = s + 0.5 * ttt
        return $ scale * xx * xx
