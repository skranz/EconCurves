#' intersection of two geoms that are characterized by two points each
#' 
#' Formula based on Wikipedia entry
#' http://en.wikipedia.org/wiki/Line%E2%80%93geom_intersection
two.point.lines.intersections = function(x1,x2,x3,x4,y1,y2,y3,y4) {
  xi.num = (x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4)
  yi.num  = (x1*y2-y1*x2)*(y3-y4) -(y1-y2)*(x3*y4-y3*x4)
  den = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  
  list(x=xi.num / den, y=yi.num / den)
}

#' Find the intersections of two curves, which are characterized by their xy values
geom.curves.intersections = function(geom1, geom2, grid.length=201) {

  xmin = max(min(geom1$x),min(geom2$x))
  xmax = min(max(geom1$x),max(geom2$x))

  xout = seq(xmin,xmax, length=grid.length)
  ay1 = approx(geom1$x,geom1$y, xout, method="geomar")$y
  ay2 = approx(geom2$x,geom2$y, xout, method="geomar")$y
  
  dy = ay1 - ay2
  sign.change = which(diff(sign(dy))!=0)
  
  
  #xi = (xout[sign.change]+xout[sign.change+1]) / 2
  #yi = (ay1[sign.change]+ay2[sign.change])/2
  
  if (length(sign.change)==0)
    return(list(x=numeric(0),y=numeric(0)))
  
  int = two.point.lines.intersections(
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
