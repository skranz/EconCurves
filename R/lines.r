
draw.line = function(line,lwd.factor=1,...) {
  lines(x=line$x,y=line$y,col=line$color,lty=line$lty,lwd=line$lwd*lwd.factor,...)
}



marker.to.geom = marker.to.line = function(marker, values, xrange,yrange, lty=2, lwd=1, color="grey", color.level=1, name.prefix = "", name.postfix = "", label.prefix="", label.postfix="", label.replace=values, pane.name="",...) {
  restore.point("computer.marker.line")
  
  fields = c("color", "color.level", "lty","lwd")
  opts = copy.into.null.fields(dest=marker[fields],source=nlist(color,color.level,lty,lwd))

  
  pos = values[[marker$var]]
  
  if (!isTRUE(is.finite(pos))) {
    warning(paste0("No finite values in param for marker ", marker$name))
    return(NULL)
  }
  
  
  if (marker$axis == "x") {
    x = c(pos,pos)
    y = yrange
  } else {
    y = c(pos,pos)
    x = xrange
  }
  color = curve.color(opts$color, level=opts$color.level)
  
  base = marker$name
  name = paste0(name.prefix, marker$name,name.postfix)
  if (is.null(marker$label)) {
    lab = name
  } else {
    lab = marker$label
    if (!is.null(label.replace)) 
      lab = replace.whiskers(lab , label.replace)
    lab = paste0(label.prefix,lab,label.postfix)
  }

  list(base=base,name=name,pane=pane.name,type="marker", geom.type="line", label=lab,axis=marker$axis,x=x,y=y,color=color, lty=opts$lty,lwd=opts$lwd)  
}

# compute.curve.line
curve.to.geom = curve.to.line = function(curve, xrange=c(0,1),yrange=c(0,1), values=list(), name.prefix = "", name.postfix = "", label.prefix=name.prefix, label.postfix=name.postfix,  label.replace=values,color.level=1,lty=1,lwd=2, color="black", pane.name="", ...) {
  restore.point("curve.to.line")
  
  fields = c("color", "color.level", "lty","lwd")
  opts = copy.into.null.fields(dest=curve[fields],source=nlist(color,color.level,lty,lwd))
  
  cu = curve
  xy = compute.curve.points(cu, xrange, yrange, values=values)
  
  if (!isTRUE((any(is.finite(xy$x+xy$y))))) {
    warning(paste0("No finite values for curve ", curve$name))
    return(NULL)
  }

  color = curve.color(opts$color, opts$color.level)  
  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  name = paste0(name.prefix, cu$name,name.postfix)
  if (is.null(curve$label)) {
    lab = name
  } else {
    lab = curve$label
    if (!is.null(label.replace))
      lab = replace.whiskers(lab , label.replace)
    lab = paste0(label.prefix,lab,label.postfix)
  }
  list(base=cu$name,name=name,pane=pane.name,type="curve",geom.type="line",label=lab,axis="",x=x,y=y,color=color, lty=opts$lty,lwd=opts$lwd)    
}



compute.curve.points = function(cu, xrange, yrange, values, xlen=101,ylen=xlen, ...) {
  restore.point("compute.curve.points")

  if (!is.data.frame(values)) {
    pdf = do.call(quick.df,as.list(values))
  } else {
    pdf = as.data.frame(values)
  }
  values = as.list(values)
  
  if (!is.null(cu$yformula_) & (!isTRUE(cu$is.vertical))) {
    if (isTRUE(cu$is.horizontal) | isTRUE(cu$is.linear)) {
      xlen=2
    }
    xseq = seq(xrange[1],xrange[2], length=xlen)
    values[[cu$xvar]] = xseq
    yseq = eval(cu$yformula_, values)
    if (length(yseq)==1) yseq <- rep(yseq,length(xseq))
    return(list(x=xseq,y=yseq))    
  }
  if (!is.null(cu$xformula_)) {
    if (isTRUE(cu$is.vertical) | isTRUE(cu$is.linear)) {
      ylen=2
    }
    yseq = seq(yrange[1],yrange[2], length=ylen)
    values[[cu$yvar]] = yseq
    xseq = eval(cu$xformula_, values)
    if (length(xseq)==1) xseq <- rep(xseq,ylen)
    return(list(x=xseq,y=yseq))
  }
  
  li = compute.implicit.z(cu, xrange, yrange, values, xlen=xlen,ylen=ylen, z.as.matrix=TRUE)
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
