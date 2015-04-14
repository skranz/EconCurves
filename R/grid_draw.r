
# Draw xy values of curve in current viewport
draw.curves.xy = function(curves, theme = default.theme()) {
  restore.point("draw.curves")
  
  for (cu in curves) {
    col = ifelse(is.null(cu$color), "black", cu$color)
    gp = gpar(col=col, lwd=theme$curve.lwd, alpha=theme$curve.alpha)
    grid.lines(x = cu$xy$x,y=cu$xy$y, default.units="native", gp=gp)
  }
}

draw.axis = function(xvar=NULL, yvar=NULL, theme=default.theme(), vp=current.viewport(),...) {
  # draw axis
  arrow = arrow(angle = 25, length = unit(0.15, "inches"),
      ends = "last", type = "closed")
  
  grid.lines(x=c(0,0),y=c(0,1), default.units="npc", gp=gpar(lwd=theme$axis.lwd),
             arrow = arrow)
  if (!is.null(yvar)) {
    grid.text(yvar, x=unit(0,"npc")-unit(1,"char"), y=unit(1,"npc"),just="right", gp=gpar(fontsize=16))
  }
  
  grid.lines(x=c(0,1),y=c(0,0), default.units="npc", gp=gpar(lwd=theme$axis.lwd),
             arrow = arrow)
  if (!is.null(yvar)) {
    grid.text(xvar, x=1, y=unit(-1,"char"), just="right", gp=gpar(fontsize=16))
  }
}