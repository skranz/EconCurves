
line.to.line.shift = function(line, ref.line, num.points=5,...) {
  restore.point("line.to.line.shift")
  n = length(line$x)
  num.points = min(num.points,n)
  if (n>1) {
    rows = unique(round(seq(1,n,length=num.points)))
  } else {
    rows = 1
  }
  row = 1
  li = lapply(rows, function(row) {
    pos = point.to.line.pos(c(x=line$x[row],y=line$y[row]), ref.line)
    sign(pos)
  })
  df = as.data.frame(do.call(rbind,li))
  if (diff(range(df$x))>=2 | diff(range(df$y))>=2){
    stop("line was not shifted but new line crosses old line")  
  }
  
  x.ind = which.max(abs(df$x))
  y.ind = which.max(abs(df$y))
  return( c(x=df$x[x.ind], y=df$y[y.ind]) )
}

point.to.line.pos = function(xy,line,dim="xy") {
  restore.point("point.to.line.pos")

  line.xy = find.nearest.line.point(xy=xy,line=line,dim=dim)
  #
  #points(xy[1],xy[2])
  #points(line.xy[1],line.xy[2],col="red")
  #lines(x=line$x,y=line$y, col="red")
  xy-line.xy
}

point.to.line.distance = function(xy,line, dim="xy") {
  restore.point("point.to.line.distance")
  
  if (line$type=="marker") {
    if (dim!="xy" & line$axis !=dim) return(Inf)
    dim = line$axis    
  }
  
  if (dim=="xy") {
    dist.vec = (xy[1]-line$x)^2+(xy[2]-line$y^2)
    dist = sqrt(min(dist.vec))
  } else if (dim=="x") {
    dist.vec = abs(xy[1]-line$x)
    dist = min(dist.vec)
  } else if (dim=="y") {
    dist.vec = abs(xy[2]-line$y)
    dist = min(dist.vec)
  }
  dist  
}

find.nearest.line.point = function(xy, line, dim="xy") {
  restore.point("find.nearests.line.point")
  xy = unlist(xy)
  if (line$type=="marker") {
    if (line$axis == "x") return(c(line$x[1],xy[2]))
    if (line$axis == "y") return(c(xy[1],line$y[1]))
  }

  if (dim=="xy") {
    dist.vec = (xy[1]-line$x)^2+(xy[2]-line$y)^2
  } else if (dim=="x") {
    dist.vec = abs(xy[1]-line$x)
  } else if (dim=="y") {
    dist.vec = abs(xy[2]-line$y)
  }
  row = which.min(dist.vec)
  c(x=line$x[row],y=line$y[row])
}


#' intersection of two lines that are characterized by two points each
#' 
#' Formula based on Wikipedia entry
#' http://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
two.point.lines.intersections = function(x1,x2,x3,x4,y1,y2,y3,y4) {
  xi.num = (x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4)
  yi.num  = (x1*y2-y1*x2)*(y3-y4) -(y1-y2)*(x3*y4-y3*x4)
  den = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  
  list(x=xi.num / den, y=yi.num / den)
}

#' Find the intersections of two curves, which are characterized by their xy values
lines.intersections = function(line1, line2, grid.length=201) {

  xmin = max(min(line1$x),min(line2$x))
  xmax = min(max(line1$x),max(line2$x))

  xout = seq(xmin,xmax, length=grid.length)
  ay1 = approx(line1$x,line1$y, xout, method="linear")$y
  ay2 = approx(line2$x,line2$y, xout, method="linear")$y
  
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

line.x.value = function(line, y) {
  approx(x=line$y,y=line$x, xout=y, method="linear")$y
}

line.y.value = function(line, x) {
  approx(x=line$x,y=line$y, xout=x, method="linear")$y
}
