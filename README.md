The R package EconCurves is part of the bundle: EconCurves, EconModels and EconStories thathelps to build interactive shiny illustrations and exercises on the web for simple or intermediate economic models. 

The EconCurves package provides infrastructure to work with curves. For example, we can

  - symbolically define curves
  - draw one or several curves in panes
  - perform computations like cutpoints between two curves
  - check user clicks relative to curves (above, below, etc)
  
Curves can be specified with a yaml syntax and then be parsed.

Main objects:
  - pane: a pane with x and y axis in which one or several curves
    can be drawn.
  - curve: a symbolic specification of a curve. Specified by an
    equation, which is tried to be solved for explicit x and y
    solutions. Can contain extra information like color scheme
    or label
  - marker: a horizontal or vertical straight line
  - data: a dataframe or list that contains parameter values
    used to compute a curve.
  - line: contains actually computed x and y values of a curve
    or marker given x and y ranges and some data.

[http://econfin.de:3838/makro/](http://econfin.de:3838/makro/)