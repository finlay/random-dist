module Plot (
    plot,
    plotDensity
) where

import Graphics.Rendering.Chart
import Data.Accessor
import Data.Colour
import Data.Colour.Names

import Plot.Utils
import Statistics.Types
import Statistics.Distribution
import qualified Data.Vector.Unboxed as V

plot :: Sample -> String -> FilePath -> IO ()
plot vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n = V.length vs

    tv, ev, dv :: [(Double, Double)]
    tv = [(fromIntegral i, i /. n) | i <- [1 .. n]]
    ev = zip (map fromIntegral [1 .. n]) (V.toList vs)
    dv = zipWith (\ (x, t) (_, e) -> (x, t - e)) tv ev

    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_plots      ^= [ Left  (toPlot theoretical),
                                     Left  (toPlot empirical),
                                     Right (toPlot difference)]
           $ layout1_right_axis ^= yaxis
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    theoretical :: PlotLines Double Double
    theoretical = theoreticalWith tv

    empirical :: PlotLines Double Double
    empirical = empiricalWith ev

    difference :: PlotLines Double Double
    difference = differenceWith $
                 zipWith (\ (x, t) (_, e) -> (x, t - e)) tv ev

yaxis :: LayoutAxis Double
yaxis  = laxis_title    ^= "theoretical - empirical"
       $ laxis_override ^= (axisGridHide . rStyleTicks)
       $ defaultLayoutAxis

rStyleTicks     :: AxisData x -> AxisData x
rStyleTicks ad  = ad{ axis_ticks_ = map invert_tick (axis_ticks_ ad) }
    where
        invert_tick :: (x,Double) -> (x, Double)
        invert_tick (x, t) = (x, -t)



plotDensity :: (ContDistr d) => d -> Sample -> String -> FilePath -> IO ()
plotDensity gd vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n  = V.length vs
    mx = V.maximum vs

    -- divide domain into bins
    bins :: Double
    bins = fromIntegral $ max (n `div` 100) 1000

    xs, ys :: [Double]
    xs = map (* (mx / bins)) [0 .. bins]
    -- count how many at each level
    ys =  let counts = count (V.toList vs) xs
              scale  = fromIntegral n * (mx / bins)
          in  map ((/ scale) . fromIntegral) counts

    layout :: Layout1 Double Double
    layout = stdLayout title
               [ Left (toPlot (  empiricalWith (zip xs ys)))
               , Left (toPlot (theoreticalWith [(x, density gd x) | x <- xs])) ]

(/.) :: (Real a, Real b, Fractional c) => a -> b -> c
(/.) x y = fromRational $ toRational x / toRational y

count :: [Double] -> [Double] -> [Int]
count samples breaks = map length (foldr go (:[]) breaks samples)
  where
    go x r s = let (ys, zs) = break (>= x) s in ys : r zs

