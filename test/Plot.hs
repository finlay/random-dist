module Plot (
    plot,
    plotDensity
) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class

import Plot.Utils
import Statistics.Types
import Statistics.Distribution
import qualified Data.Vector.Unboxed as V

width, height :: Int
width = 800
height = 400
file_opts :: FileOptions
file_opts = fo_size .~ (width, height)
          $ fo_format .~ SVG
          $ def

plot :: Sample -> String -> FilePath -> IO (PickFn ())
plot vs title fn = renderableToFile file_opts fn (toRenderable layout)
  where
    n = V.length vs

    tv, ev, dv :: [(Double, Double)]
    tv = [(fromIntegral i, i /. n) | i <- [1 .. n]]
    ev = zip (map fromIntegral [1 .. n]) (V.toList vs)
    dv = zipWith (\ (x, t) (_, e) -> (x, t - e)) tv ev

    layout :: LayoutLR Double Double Double 
    layout = layoutlr_title      .~ title
           $ layoutlr_background .~ solidFillStyle (opaque white)
           $ layoutlr_plots      .~ [ Left (toPlot theoretical),
                                      Left (toPlot empirical),
                                      Right (toPlot difference)]
           $ layoutlr_right_axis .~ yaxis
           $ layoutlr_foreground .~ opaque black
           $ def

    theoretical :: PlotLines Double Double
    theoretical = theoreticalWith tv

    empirical :: PlotLines Double Double
    empirical = empiricalWith ev

    difference :: PlotLines Double Double
    difference = differenceWith dv

yaxis :: LayoutAxis Double
yaxis  = laxis_title    .~ "theoretical - empirical"
       $ laxis_override .~ (axisGridHide . rStyleTicks)
       $ def

rStyleTicks     :: AxisData x -> AxisData x
rStyleTicks ad  = ad{ _axis_ticks = map invert_tick (_axis_ticks ad) }
    where
        invert_tick :: (x,Double) -> (x, Double)
        invert_tick (x, t) = (x, -t)



plotDensity :: (ContDistr d) => d -> Sample -> String -> FilePath -> IO (PickFn ())
plotDensity gd vs title fn = renderableToFile file_opts fn (toRenderable layout)
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
              scale'  = fromIntegral n * (mx / bins)
          in  map ((/ scale') . fromIntegral) counts

    layout :: LayoutLR Double Double Double
    layout = stdLayout title
               [ Left (toPlot (  empiricalWith (zip xs ys)))
               , Left (toPlot (theoreticalWith [(x, density gd x) | x <- xs]) )]

(/.) :: (Real a, Real b, Fractional c) => a -> b -> c
(/.) x y = fromRational $ toRational x / toRational y

count :: [Double] -> [Double] -> [Int]
count samples breaks = map length (foldr go (:[]) breaks samples)
  where
    go x r s = let (ys, zs) = break (>= x) s in ys : r zs

