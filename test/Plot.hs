module Plot (
    plot,
    plotDensity
) where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor

import Statistics.Types
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
line   = line_width       ^= 0.8
       $ defaultPlotLines ^. plot_lines_style

lineWith :: CairoLineStyle -> String -> [(Double, Double)] ->
            PlotLines Double Double
lineWith ls txt vs = plot_lines_style ^= ls
       $ plot_lines_values   ^= [vs]
       $ plot_lines_title    ^= txt
       $ defaultPlotLines

theoreticalWith :: [(Double, Double)] -> PlotLines Double Double
theoreticalWith = lineWith line_t "Theoretical"

empiricalWith :: [(Double, Double)] -> PlotLines Double Double
empiricalWith = lineWith line_e "Empirical"

differenceWith :: [(Double, Double)] -> PlotLines Double Double
differenceWith = lineWith line_d "Difference"

plotDensity :: Sample -> String -> FilePath -> IO ()
plotDensity vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n = V.length vs
    mx = V.maximum vs

    -- divide domain into bins
    bins :: Double
    bins = fromIntegral $ max (n `div` 100) 1000

    x :: [Double]
    x = map (* ((mx * 1.2) / bins)) [0 .. bins]

    -- count how many at each level
    y :: [Double]
    y = map (/. n) $ snd $ V.foldr cumulate (x, [0]) vs

    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_plots      ^= [ Left  (toPlot empirical) ]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    empirical :: PlotLines Double Double
    empirical = empiricalWith (zip x y)


cumulate :: Double -> ([Double], [Double]) -> ([Double], [Double])
cumulate _ ([], ys) = ([], ys)
cumulate y' (x1:[], ys)
        | y' < x1   = ([], ((head ys + 1) : tail ys))
        | otherwise = ([], ys)
cumulate y' (x1:x2:xs, ys)
        | y' < x1   = (xs, ((head ys + 1) : tail ys))
        | y' < x2   = ((x2:xs), (1:ys))
        | otherwise = cumulate y' ((x2:xs), (0:ys))
