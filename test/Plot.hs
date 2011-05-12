module Plot ( plot ) where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor


chart :: String -> [(Double, Double)] -> [(Double, Double)] -> Renderable ()
chart title tv ev = toRenderable layout
  where
    layout :: Layout1 Double Double
    layout = layout1_title      ^= title
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_left_axis  ^: laxis_override ^= axisTicksHide
           $ layout1_plots      ^= [ Left (toPlot theoretical),
                                     Left (toPlot empirical),
                                     Right (toPlot difference)]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    theoretical = plot_lines_style  ^= line_t
           $ plot_lines_values  ^= [tv]
           $ plot_lines_title   ^= "Theoretical"
           $ defaultPlotLines

    empirical = plot_lines_style ^= line_e
           $ plot_lines_values  ^= [ev]
           $ plot_lines_title   ^= "Empirical"
           $ defaultPlotLines

    difference = plot_lines_style ^= line_d
           $ plot_lines_values  ^= [[ (x, t-e) | ((x,t),(_,e)) <- zip tv ev]]
           $ plot_lines_title   ^= "Difference"
           $ defaultPlotLines

    line_e = line_color ^= opaque red   $ line
    line_t = line_color ^= opaque blue  $ line
    line_d = line_color ^= opaque green $ line

    line   = line_width       ^= 0.8 
           $ defaultPlotLines ^. plot_lines_style

plot :: [(Double, Double)] -> [(Double, Double)] -> String -> FilePath -> IO ()
plot tv ev title fn = renderableToSVGFile (chart title tv ev) 800 600 fn
