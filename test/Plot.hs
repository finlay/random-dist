module Plot ( plot ) where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor

import Statistics.Types
import qualified Data.Vector.Unboxed as V

(/.) :: (Real a, Real b, Fractional c) => a -> b -> c
(/.) x y = fromRational $ (toRational x) / (toRational y)

chart :: String -> Sample -> Renderable ()
chart title vs = toRenderable layout
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

plot :: Sample -> String -> FilePath -> IO ()
plot vs title fn = renderableToSVGFile (chart title vs) 800 600 fn
