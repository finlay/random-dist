{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Statistics.Distribution.Random.Exponential (
    exponential
) where

import Random.CRI
import Statistics.Distribution.Random.Uniform

-- q[k-1] = sum(log(2)^k / k!)  k=1,..,n, 
q :: [Double]
-- q = let factorial = foldr (*) 1 . enumFromTo 1
--         qk :: Integer -> Double
--         qk k = (log 2)^k / (fromIntegral (factorial k))
--         qs :: [Double]
--         qs = map qk [1..]
--     in map (\ n -> sum (take n qs)) (take 16 [1..])
q = [0.6931471805599453,
     0.9333736875190459,
     0.9888777961838675,
     0.9984959252914960,
     0.9998292811061389,
     0.9999833164100727,
     0.9999985691438767,
     0.9999998906925558,
     0.9999999924734159,
     0.9999999995283275,
     0.9999999999728814,
     0.9999999999985598,
     0.9999999999999289,
     0.9999999999999968,
     0.9999999999999999,
     1.0000000000000000]
{-# INLINE q #-}

exponential :: Source m g Double => g m -> m Double
exponential rng = 
    let !q0 = q !! 0
        mk_au :: Double -> Double -> (Double, Double)
        mk_au !a !u  
            | u > 1.0   = (a, u - 1.0)
            | otherwise = mk_au (a + q0) (u + u)

        go a _ umin ![] = return (a + umin * q0) 
        go a u umin !(qi:qs) = do 
            !ustar <- uniform rng
            let umin' = min ustar umin
            if u > qi 
                then go a u umin' qs
                else return (a + umin' * q0)
    in do
        !u' <- uniform rng
        let !(a, u) = mk_au 0.0 (u' + u')
        if u <= q0
            then return (a + u)
            else do
                !us <- uniform rng
                go a u us (tail q)

