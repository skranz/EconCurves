
draw.curve = function(geom,lwd.factor=1,...) {
  restore.point("draw.curve")
  
  lines(x=geom$x,y=geom$y,col=geom$color,lty=geom$lty,lwd=geom$lwd*lwd.factor,...)
}

gcurve.slopes = function(gcurve) {
  diff(gcurve$y) / diff(gcurve$x)
}

marker.to.geom = function(marker, values, xrange,yrange, lty=2, lwd=1, color="grey", color.level=1, name.prefix = "", name.postfix = "", label.prefix="", label.postfix="", label.replace=values, pane.name="",...) {
  restore.point("computer.marker.geom")
  
  fields = c("color", "color.level", "lty","lwd")
  opts = copy.into.null.fields(dest=marker[fields],source=nlist(color,color.level,lty,lwd))

  
  pos = values[[marker$var]]
  
  if (is.null(pos)) {
    msg = paste0("Value of ",marker$var," missing for marker ", marker$name)
    warning(msg); cat(msg)
    return(NULL)
  }
  
  if (!isTRUE(is.finite(pos))) {
    msg = paste0("No finite values in param for marker ", marker$name)
    warning(msg); cat(msg)
    return(NULL)
  }
  
  
  if (marker$axis == "x") {
    x = c(pos,pos)
    y = yrange
  } else {
    y = c(pos,pos)
    x = xrange
  }
  base = marker$name
  name = paste0(name.prefix, marker$name,name.postfix)
  
  color = geom.color(base.color = opts$color, color.level=opts$color.level)
  label = geom.label(label.prefix = label.prefix, label.postfix=label.postfix,label.replace=label.replace,label = marker$label,name = name)
  
  list(base=base,name=name,pane=pane.name,type="marker", geom.type="gcurve", label=label,axis=marker$axis,x=x,y=y,color=color, lty=opts$lty,lwd=opts$lwd)  
}

# compute.curve.gcurve
curve.to.geom = function(curve, xrange=c(0,1),yrange=c(0,1), values=list(), name.prefix = "", name.postfix = "", label.prefix=name.prefix, label.postfix=name.postfix,  label.replace=values,color.level=1,lty=1,lwd=2, color="black", pane.name="", xlen=201, ylen=201, ...) {
  restore.point("curve.to.geom")
  
  fields = c("color", "color.level", "lty","lwd")
  opts = copy.into.null.fields(dest=curve[fields],source=nlist(color,color.level,lty,lwd))
  
  cu = curve
  xy = compute.curve.points(cu, xrange, yrange, values=values,xlen=xlen,ylen=ylen)
  
  if (!isTRUE((any(is.finite(xy$x+xy$y))))) {
    warning(paste0("No finite values for curve ", curve$name))
    return(NULL)
  }

  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  name = paste0(name.prefix, cu$name,name.postfix)
  
  color = geom.color(base.color = opts$color, color.level=opts$color.level)
  label = geom.label(label.prefix = label.prefix, label.postfix=label.postfix,label.replace=label.replace,label = cu$label)
  
  list(base=cu$name,name=name,pane=pane.name,type="curve",geom.type="gcurve",label=label,axis="",x=x,y=y,color=color, lty=opts$lty,lwd=opts$lwd)    
}

compute.curve.grid = function(cu=geom$obj, values=geom$values, xrange=geom$xrange,yrange=geom$yrange, xlen=geom$xlen,ylen=geom$ylen, dim="x",x=geom$x, y=geom$y, geom=NULL) {
  restore.point("compute.curve.grid")
  
  if (dim=="x") {
    xseq = seq(xrange[1], xrange[2], length=xlen)
    if (isTRUE(cu$is.vertical)) {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen)
      xy$x = round.to.grid(xy$x,length=xlen, range=xrange)
      return(xy)
    } else if (!is.null(cu$yformula_)) {
      values[[cu$xvar]] = xseq
      yseq = eval(cu$yformula_, values)
      if (length(yseq)==1) yseq <- rep(yseq,length(xseq))
      return(list(x=xseq,y=yseq))  

    } else if (!is.null(x) & !is.null(y)) {
      if (is.null(geom))
        geom = list(x=x,y=y, xrange=xrange, yrange=yrange,
                    xlen=xlen,ylen=ylen)
      return(compute.geom.grid(geom=geom,dim = dim,use.object = FALSE))
    } else {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen, use.xformula=FALSE)
      return(xy)
    }
  }

  if (dim=="y") {
    yseq = seq(yrange[1], yrange[2], length=xlen)
    if (isTRUE(cu$is.horizontal)) {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen)
      xy$y = round.to.grid(xy$y,length=ylen, range=yrange)
      return(xy)
    } else if (!is.null(cu$xformula_)) {
      values[[cu$yvar]] = yseq
      xseq = eval(cu$xformula_, values)
      if (length(xseq)==1) xseq <- rep(xseq,length(yseq))
      return(list(x=xseq,y=yseq))  

    } else if (!is.null(x) & !is.null(y)) {
      if (is.null(geom))
        geom = list(x=x,y=y, xrange=xrange, yrange=yrange,
                    xlen=xlen,ylen=ylen)
      return(compute.geom.grid(geom=geom,dim = dim,use.object = FALSE))
    } else {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen, use.yformula=FALSE)
      return(xy)
    }
  }
   
  
}

compute.curve.points = function(cu, xrange, yrange, values, xlen=101,ylen=xlen, use.xformula=TRUE, use.yformula=TRUE, ...) {
  restore.point("compute.curve.points")

  #if (is.null(values)) values=list()
  values = as.list(values)
  
  if (isTRUE(cu$is.linear) & (!cu$is.vertical) & (!cu$is.horizontal)) {
    # need to add both x and y range to have at least 
    # 2 points inside the pane
    xseq = seq(xrange[1],xrange[2], length=2)
    values[[cu$xvar]] = xseq
    yval = eval(cu$yformula_, values)

    yseq = seq(yrange[1],yrange[2], length=2)
    values[[cu$yvar]] = yseq
    xval = eval(cu$xformula_, values)

    return(list(x=c(xseq,xval),y=c(yval,yseq)))    
  }

  
  if (!is.null(cu$yformula_) & (!isTRUE(cu$is.vertical)) & use.yformula) {
    if (isTRUE(cu$is.horizontal) | isTRUE(cu$is.linear)) {
      xlen=2
    }
    xseq = seq(xrange[1],xrange[2], length=xlen)
    values[[cu$xvar]] = xseq
    yseq = eval(cu$yformula_, values)
    if (length(yseq)==1) yseq <- rep(yseq,length(xseq))
    return(list(x=xseq,y=yseq))    
  }
  if (!is.null(cu$xformula_) & use.xformula) {
    if (isTRUE(cu$is.vertical) | isTRUE(cu$is.linear)) {
      ylen=2
    }
    yseq = seq(yrange[1],yrange[2], length=ylen)
    values[[cu$yvar]] = yseq
    xseq = eval(cu$xformula_, values)
    if (length(xseq)==1) xseq <- rep(xseq,ylen)
    return(list(x=xseq,y=yseq))
  }
  
  li = compute.curve.implicit.z(cu, xrange, yrange, values, xlen=xlen,ylen=ylen, z.as.matrix=TRUE)
  options("max.contour.segments" =xlen) 
  res = contourLines(li$xseq,li$yseq,li$z, level = 0)
  if (length(res)==0) {
    res = NULL
  } else {
    res = res[[1]]
  }
  return(list(x = res$x, y=res$y))
}

compute.curve.implicit.z = function(cu, xrange, yrange,par,  xlen=101,ylen=xlen, z.as.matrix=FALSE) {
  restore.point("compute.implicit")
  
  # Compute a contour gcurve using the implicit function
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
