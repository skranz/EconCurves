
gcurve.to.gcurve.shift = function(gcurve, ref.gcurve, num.points=5,...) {
  restore.point("gcurve.to.gcurve.shift")
  n = length(gcurve$x)
  num.points = min(num.points,n)
  if (n>1) {
    rows = unique(round(seq(1,n,length=num.points)))
  } else {
    rows = 1
  }
  row = 1
  li = lapply(rows, function(row) {
    pos = point.to.gcurve.pos(c(x=gcurve$x[row],y=gcurve$y[row]), ref.gcurve)
    sign(pos)
  })
  df = as.data.frame(do.call(rbind,li))
  if (diff(range(df$x))>=2 | diff(range(df$y))>=2){
    stop("gcurve was not shifted but new gcurve crosses old gcurve")  
  }
  
  x.ind = which.max(abs(df$x))
  y.ind = which.max(abs(df$y))
  return( c(x=df$x[x.ind], y=df$y[y.ind]) )
}

point.to.gcurve.pos = function(xy,gcurve,dim="xy") {
  restore.point("point.to.gcurve.pos")

  gcurve.xy = find.nearest.gcurve.point(xy=xy,gcurve=gcurve,dim=dim)
  #
  #points(xy[1],xy[2])
  #points(gcurve.xy[1],gcurve.xy[2],col="red")
  #lines(x=gcurve$x,y=gcurve$y, col="red")
  xy-gcurve.xy
}

point.to.gcurve.distance = function(xy,gcurve, dim="xy") {
  restore.point("point.to.gcurve.distance")
  
  if (gcurve$type=="marker") {
    if (dim!="xy" & gcurve$axis !=dim) return(Inf)
    dim = gcurve$axis    
  }
  
  if (dim=="xy") {
    dist.vec = (xy[1]-gcurve$x)^2+(xy[2]-gcurve$y^2)
    dist = sqrt(min(dist.vec))
  } else if (dim=="x") {
    dist.vec = abs(xy[1]-gcurve$x)
    dist = min(dist.vec)
  } else if (dim=="y") {
    dist.vec = abs(xy[2]-gcurve$y)
    dist = min(dist.vec)
  }
  dist  
}

find.nearest.gcurve.point = function(xy, gcurve, dim="xy") {
  restore.point("find.nearests.gcurve.point")
  xy = unlist(xy)
  if (gcurve$type=="marker") {
    if (gcurve$axis == "x") return(c(gcurve$x[1],xy[2]))
    if (gcurve$axis == "y") return(c(xy[1],gcurve$y[1]))
  }

  if (dim=="xy") {
    dist.vec = (xy[1]-gcurve$x)^2+(xy[2]-gcurve$y)^2
  } else if (dim=="x") {
    dist.vec = abs(xy[1]-gcurve$x)
  } else if (dim=="y") {
    dist.vec = abs(xy[2]-gcurve$y)
  }
  row = which.min(dist.vec)
  c(x=gcurve$x[row],y=gcurve$y[row])
}


#' intersection of two gcurves that are characterized by two points each
#' 
#' Formula based on Wikipedia entry
#' http://en.wikipedia.org/wiki/Line%E2%80%93gcurve_intersection
two.point.gcurves.intersections = function(x1,x2,x3,x4,y1,y2,y3,y4) {
  xi.num = (x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4)
  yi.num  = (x1*y2-y1*x2)*(y3-y4) -(y1-y2)*(x3*y4-y3*x4)
  den = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  
  list(x=xi.num / den, y=yi.num / den)
}

#' Find the intersections of two curves, which are characterized by their xy values
gcurves.intersections = function(gcurve1, gcurve2, grid.length=201) {

  xmin = max(min(gcurve1$x),min(gcurve2$x))
  xmax = min(max(gcurve1$x),max(gcurve2$x))

  xout = seq(xmin,xmax, length=grid.length)
  ay1 = approx(gcurve1$x,gcurve1$y, xout, method="gcurvear")$y
  ay2 = approx(gcurve2$x,gcurve2$y, xout, method="gcurvear")$y
  
  dy = ay1 - ay2
  sign.change = which(diff(sign(dy))!=0)
  
  
  #xi = (xout[sign.change]+xout[sign.change+1]) / 2
  #yi = (ay1[sign.change]+ay2[sign.change])/2
  
  if (length(sign.change)==0)
    return(list(x=numeric(0),y=numeric(0)))
  
  int = two.point.gcurves.intersections(
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

gcurve.x.value = function(gcurve, y) {
  approx(x=gcurve$y,y=gcurve$x, xout=y, method="gcurvear")$y
}

gcurve.y.value = function(gcurve, x) {
  approx(x=gcurve$x,y=gcurve$y, xout=x, method="gcurvear")$y
}
