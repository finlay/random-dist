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
(/.) x y = fromRational $ (toRational x) / (toRational y)

plot :: Sample -> String -> FilePath -> IO ()
plot vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n = V.length vs
    tv :: [ ( Double, Double ) ]
    tv = [ (fromIntegral i, i /. n) | i <- [1 .. n]]
    ev :: [ ( Double, Double ) ]
    ev = zip (map fromIntegral [1 .. n]) (V.toList vs)

    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_plots      ^= [ Left  (toPlot theoretical),
                                     Left  (toPlot empirical),
                                     Right (toPlot difference)]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    theoretical = plot_lines_style  ^= line_t
           $ plot_lines_values      ^= [tv]
           $ plot_lines_title       ^= "Theoretical"
           $ defaultPlotLines

    empirical = plot_lines_style ^= line_e
           $ plot_lines_values   ^= [ev]
           $ plot_lines_title    ^= "Empirical"
           $ defaultPlotLines

    difference = plot_lines_style ^= line_d
           $ plot_lines_values    ^= [[ (x, t-e) | ((x,t),(_,e)) <- zip tv ev]]
           $ plot_lines_title     ^= "Difference"
           $ defaultPlotLines

    line_e = line_color ^= opaque red   $ line
    line_t = line_color ^= opaque blue  $ line
    line_d = line_color ^= opaque green $ line

    line   = line_width       ^= 0.8
           $ defaultPlotLines ^. plot_lines_style


plotDensity :: Sample -> String -> FilePath -> IO ()
plotDensity vs title fn = renderableToSVGFile (toRenderable layout) 800 600 fn
  where
    n = V.length vs
    mx = V.maximum vs

    -- divide domain into bins 
    bins = fromIntegral $ max (n `div` 100) 1000
    x = map (* ((mx*1.2) / bins)) [ 0.0, 1.0 .. bins ]

    -- count how many at each level
    y = map (/. n) $ snd $ foldr cumulate (x, [0]) $ V.toList vs
         where 
            cumulate :: Double -> ([Double], [Double]) -> ([Double], [Double])
            cumulate _ ([], ys) = ([], ys)
            cumulate y' (x1:[], ys) 
                    | y' < x1   = ([], ((head ys + 1) : tail ys))
                    | otherwise = ([], ys)
            cumulate y' (x1:x2:xs, ys) 
                    | y' < x1   = (xs, ((head ys + 1) : tail ys))
                    | y' < x2   = ((x2:xs), (1:ys))
                    | otherwise = cumulate y' ((x2:xs), (0:ys)) 

    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_plots      ^= [ Left  (toPlot empirical) ]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    empirical = plot_lines_style ^= line_e
           $ plot_lines_values   ^= [zip x y]
           $ plot_lines_title    ^= "Empirical"
           $ defaultPlotLines

    line_e = line_color ^= opaque red   $ line
--     line_t = line_color ^= opaque blue  $ line
--     line_d = line_color ^= opaque green $ line

    line   = line_width       ^= 0.8
           $ defaultPlotLines ^. plot_lines_style
    
