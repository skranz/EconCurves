
#' intersection of two lines that are characterized by two points each
#' 
#' Formula based on Wikipedia entry
#' http://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
lines.intersections.two.points = function(x1,x2,x3,x4,y1,y2,y3,y4) {
  xi.num = (x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4)
  yi.num  = (x1*y2-y1*x2)*(y3-y4) -(y1-y2)*(x3*y4-y3*x4)
  den = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  
  list(x=xi.num / den, y=yi.num / den)
}

#' Find the intersections of two curves, which are characterized by their xy values
curves.xy.intersections = function(xy1, xy2, grid.length=201) {
  restore.point("curves.xy.intersections")
  xmin = max(min(xy1$x),min(xy2$x))
  xmax = min(max(xy1$x),max(xy2$x))

  #ymin = max(min(xy1$y),min(xy2$y))
  #ymax = min(max(xy1$y),max(xy2$y))

  xout = seq(xmin,xmax, length=grid.length)
  ay1 = approx(xy1$x,xy1$y, xout, method="linear")$y
  ay2 = approx(xy2$x,xy2$y, xout, method="linear")$y
  
  dy = ay1 - ay2
  sign.change = which(diff(sign(dy))!=0)
  
  
  #xi = (xout[sign.change]+xout[sign.change+1]) / 2
  #yi = (ay1[sign.change]+ay2[sign.change])/2
  
  if (length(sign.change)==0)
    return(list(x=numeric(0),y=numeric(0)))
  
  int = lines.intersections.two.points(
    x1 = xout[sign.change],
    x2 = xout[sign.change+1],
    y1 = ay1[sign.change],
    y2 = ay1[sign.change+1],
    x3 = xout[sign.change],
    x4 = xout[sign.change+1],
    y3 = ay2[sign.change],
    y4 = ay2[sign.change+1]
  )
  
  return(int)
}

xy.curve.x.value = function(xy, y) {
  approx(x=xy$y,y=xy$x, xout=y, method="linear")$y
}

xy.curve.y.value = function(xy, x) {
  approx(x=xy$x,y=xy$y, xout=x, method="linear")$y
}

#' Get the x-value of an curve
curve.x.value = function(cu, y) {
  approx(x=cu$xy$y,y=cu$xy$x, xout=y, method="linear")$y
}

#' Get the y-value of an curve
curve.y.value = function(xy, x) {
  approx(x=xy$x,y=xy$y, xout=x, method="linear")$y
}

#' Find the intersections of two curves on a pane
curves.intersections = function(cu1, cu2, grid.length=201) {
  restore.point("curves.xy.intersections")
  xmin = max(min(cu1$xrange),min(cu2$xrange))
  xmax = min(max(cu1$xrange),max(cu2$xrange))

  # Swap to make cu1 vertical
  if (!is.vertical(cu2)) {
    cu3 = cu1; cu1=cu2; cu2=cu3;
  }
  
  if (is.vertical(cu1)) {
    x = cu1$xrange[1]
    y = cu2$approx.fun(x)
    return(list(x=x,y=y))
  }

  xout = seq(xmin,xmax, length=grid.length)
  ay1 = cu1$approx.fun(xout)$y
  ay2 = cu2$approx.fun(xout)$y
  
  dy = ay1 - ay2
  sign.change = which(diff(sign(dy))!=0)
    
  if (length(sign.change)==0)
    return(list(x=numeric(0),y=numeric(0)))
  
  int = lines.intersections.two.points(
    x1 = xout[sign.change],
    x2 = xout[sign.change+1],
    y1 = ay1[sign.change],
    y2 = ay1[sign.change+1],
    x3 = xout[sign.change],
    x4 = xout[sign.change+1],
    y3 = ay2[sign.change],
    y4 = ay2[sign.change+1]
  )
  
  return(int)
}

compute.curve.points = function(cu, xrange, yrange, par,xlen = 101,ylen=xlen, ...) {
  restore.point("compute.curve.points")

  if (!is.data.frame(par)) {
    pdf = do.call(quick.df,as.list(par))
  } else {
    pdf = as.data.frame(par)
  }
  par = as.list(par)
  if (!is.null(cu$yformula_)) {
    xseq = seq(xrange[1],xrange[2], length=xlen)
    par[[cu$xvar]] = xseq
    yseq = eval(cu$yformula_, par)
    if (length(yseq)==1) yseq <- rep(yseq,xlen)
    return(list(x=xseq,y=yseq))    
  }
  if (!is.null(cu$xformula_)) {
    yseq = seq(yrange[1],yrange[2], length=ylen)
    par[[cu$yvar]] = yseq
    xseq = eval(cu$xformula_, par)
    if (length(xseq)==1) xseq <- rep(xseq,ylen)
    return(list(x=xseq,y=yseq))
  }
  
  li = compute.implicit.z(cu, xrange, yrange, par, xlen=xlen,ylen=ylen, z.as.matrix=TRUE)
  options("max.contour.segments" =xlen) 
  res = contourLines(li$xseq,li$yseq,li$z, level = 0)
  if (length(res)==0) {
    res = NULL
  } else {
    res = res[[1]]
  }
  return(list(x = res$x, y=res$y))
}

compute.implicit.z = function(cu, xrange, yrange,par,  xlen=101,ylen=xlen, z.as.matrix=FALSE) {
  restore.point("compute.implicit")
  
  # Compute a contour line using the implicit function
  xseq = seq(xrange[1],xrange[2], length=xlen)
  yseq = seq(yrange[1],yrange[2], length=ylen)
  grid = expand.grid(list(x=xseq,y=yseq))

  par[[cu$xvar]] = grid$x
  par[[cu$yvar]] = grid$y
  grid$z = eval(cu$implicit_, par)
  
  if (z.as.matrix) {
    z = matrix(grid$z, nrow=length(xseq), ncol=length(yseq))
    return(list(xseq=xseq, yseq=yseq, z=z))
  }
  grid
}

is.vertical = function(cu) {
  cu$is.vertical
}

is.horizontal = function(cu) {
  cu$is.horizontal
}


specialize.curve.formula = function(eq, xvar, yvar, level=NULL) {
  restore.point("specizalize.curve.formula")
  formula_ = eq
  lhs_ = get.lhs(formula_)
  lhs = deparse1(lhs_)
  rhs_ = get.rhs(formula_)
  
  vl = find.variables(lhs_)
  vr = find.variables(rhs_)

  yformula_ = xformula_ = NULL
  
  # y variable is alone on lhs
  if (identical(lhs,yvar) & (! yvar %in% vr)) {
    yformula_ = substitute(rhs, list(rhs=rhs_))
  }

  # x variable is alone on lhs
  if (identical(lhs,xvar) & (! xvar %in% vr)) {
    xformula_ = substitute(rhs, list(rhs=rhs_))
  }
  
  # implicit formula
  implicit_ = substitute(lhs-(rhs), list(lhs=lhs_,rhs=rhs_))

  ret = nlist(xformula_, yformula_, implicit_)
  ret
}

