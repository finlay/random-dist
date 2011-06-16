module Plot.Utils where

import Data.Colour
import Data.Colour.Names
import Data.Accessor
import Graphics.Rendering.Chart

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

theoreticalWith :: [(Double, a)] -> PlotLines Double a
theoreticalWith = lineWith line_t "Theoretical"

empiricalWith :: [(Double, a)] -> PlotLines Double a
empiricalWith = lineWith line_e "Empirical"

differenceWith :: [(Double, a)] -> PlotLines Double a
differenceWith = lineWith line_d "Difference"

stdLayout :: (PlotValue x, PlotValue y) =>
             String -> [Either (Plot x y) (Plot x y)] -> Layout1 x y
stdLayout title plots =
      layout1_title      ^= title
    $ layout1_background ^= solidFillStyle (opaque white)
    $ layout1_plots      ^= plots
    $ setLayout1Foreground (opaque black)
    $ defaultLayout1
