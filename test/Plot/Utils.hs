module Plot.Utils where

import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart


line_e, line_t, line_d :: LineStyle
line_e = line_color .~ withOpacity red   0.8 $ line
line_t = line_color .~ withOpacity blue  0.8 $ line
line_d = line_color .~ withOpacity green 0.8 $ line

line :: LineStyle
line   = line_width       .~ 1.8
       $ def 

lineWith :: LineStyle -> String -> [(Double, a)] ->
            PlotLines Double a
lineWith ls txt vs = plot_lines_style .~ ls
       $ plot_lines_values   .~ [vs]
       $ plot_lines_title    .~ txt
       $ def

theoreticalWith :: [(Double, Double)] -> PlotLines Double Double
theoreticalWith = lineWith line_t "Theoretical"

empiricalWith :: [(Double, a)] -> PlotLines Double a
empiricalWith = lineWith line_e "Empirical"

differenceWith :: [(Double, Double)] -> PlotLines Double Double
differenceWith = lineWith line_d "Difference"

stdLayout :: (PlotValue x, PlotValue y) =>
             String -> [Either (Plot x y) (Plot x y)] -> LayoutLR x y y
stdLayout title plots =
      layoutlr_title      .~ title
    $ layoutlr_background .~ solidFillStyle (opaque white)
    $ layoutlr_plots      .~ plots
    $ layoutlr_foreground .~ opaque black
    $ def
