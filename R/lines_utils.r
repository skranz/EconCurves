
line.to.line.shift = function(line, ref.line, num.points=5,...) {
  restore.point("line.to.line.shift")
  n = length(line$x)
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

