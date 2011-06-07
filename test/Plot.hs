module Plot (
    plot,
    plotDensity
) where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor

import Statistics.Types
import Statistics.Distribution
import qualified Data.Vector.Unboxed as V

(/.) :: (Real a, Real b, Fractional c) => a -> b -> c
(/.) x y = fromRational $ toRational x / toRational y

plot :: Sample -> String -> FilePath -> IO ()
plot vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n = V.length vs

    tv :: [(Double, Double)]
    tv = [(fromIntegral i, i /. n) | i <- [1 .. n]]

    ev :: [(Double, Double)]
    ev = zip (map fromIntegral [1 .. n]) (V.toList vs)

    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_plots      ^= [ Left  (toPlot theoretical),
                                     Left  (toPlot empirical),
                                     Right (toPlot difference)]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    theoretical :: PlotLines Double Double
    theoretical = theoreticalWith tv

    empirical :: PlotLines Double Double
    empirical = empiricalWith ev

    difference :: PlotLines Double Double
    difference = differenceWith $
                 zipWith (\ (x, t) (_, e) -> (x, t - e)) tv ev

line_e, line_t, line_d :: CairoLineStyle
line_e = line_color ^= opaque red   $ line
line_t = line_color ^= opaque blue  $ line
line_d = line_color ^= opaque green $ line

line :: CairoLineStyle
line   = line_width       ^= 1.8
       $ defaultPlotLines ^. plot_lines_style

lineWith :: CairoLineStyle -> String -> [(Double, a)] ->
            PlotLines Double a
lineWith ls txt vs = plot_lines_style ^= ls
       $ plot_lines_values   ^= [vs]
       $ plot_lines_title    ^= txt
       $ defaultPlotLines

theoreticalWith :: [(Double, Double)] -> PlotLines Double Double
theoreticalWith = lineWith line_t "Theoretical"

empiricalWith :: [(Double, a)] -> PlotLines Double a
empiricalWith = lineWith line_e "Empirical"

differenceWith :: [(Double, Double)] -> PlotLines Double Double
differenceWith = lineWith line_d "Difference"

plotDensity :: (ContDistr d) => d -> Sample -> String -> FilePath -> IO ()
plotDensity gd vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n  = V.length vs
    mx = V.maximum vs

    -- divide domain into bins
    bins :: Double
    bins = fromIntegral $ max (n `div` 100) 1000

    xs :: [Double]
    xs = map (* (mx / bins)) [0 .. bins]

    -- count how many at each level
    ys :: [Double]
    ys =  let counts = count (V.toList vs) xs 
              scale  =  fromIntegral n * (mx / bins)
          in  map ( / scale ) $ map fromIntegral counts

    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_plots      ^= [ Left  (toPlot empirical) 
                                   , Left  (toPlot theoretical) ]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    empirical :: PlotLines Double Double
    empirical = empiricalWith (zip xs ys)

    theoretical :: PlotLines Double Double
    theoretical = theoreticalWith ([ (x, density gd x) | x <- xs ])


count :: [Double] -> [Double] -> [Int]
count samples breaks = map length (foldr go (:[]) breaks samples)
  where
    go x r s = let (ys, zs) = break (>= x) s in ys : r zs

