
draw.line = function(line,lwd.factor=1,...) {
  lines(x=line$x,y=line$y,col=line$color,lty=line$lty,lwd=line$lwd*lwd.factor,...)
}

draw.lines = function(lines,...) {
  lapply(lines, draw.line,...)
}



compute.marker.line = function(marker, params, xrange,yrange, lty=2, lwd=1, t=0, pane.name="", level=1) {
  restore.point("computer.marker.line")
  name = marker$name
  pos = params[[name]]
  
  if (marker$axis == "x") {
    x = c(pos,pos)
    y = yrange
  } else {
    y = c(pos,pos)
    x = xrange
  }
  color = curve.color("black", level=level)
  
  m = marker$name
  lab = ifelse(str.ends.with(m,"_"),substring(m,1,nchar(m)-1),m)
  lab.t = ifelse(str.starts.with(m,"lag_"),t-1,t)
  lab = ifelse(str.starts.with(m,"lag_"),str.right.of(lab,"lag_"),lab)
  lab = paste0(lab,lab.t)
  
  list(base=name,name=lab,pane=pane.name,t=t,type="marker",label=lab,axis=marker$axis,x=x,y=y,color=color, lty=lty,lwd=lwd)  
}

# compute.curve.line
curve.to.line = function(curve, xrange=c(0,1),yrange=c(0,1), params=list(), name.addon = "", label.replace=NULL, color.level=1,lty=1,lwd=2, pane.name="") {
  restore.point("curve.to.line")
  cu = curve
  xy = compute.curve.points(cu, xrange, yrange, params=params)
  color = curve.color(cu$color, color.level)  
  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  name = paste0(cu$name,name.addon)
  if (is.null(curve$label)) {
    lab = name
  } else {
    lab = curve$label
    if (!is.null(label.replace))
      lab = replace.whiskers(lab , label.replace)
  }
  list(base=cu$name,name=name,pane=pane.name,type="curve",lab=lab,axis="",x=x,y=y,color=color, lty=lty,lwd=lwd)    
}



compute.curve.points = function(cu, xrange, yrange, params,xlen = 101,ylen=xlen, ...) {
  restore.point("compute.curve.points")

  if (!is.data.frame(params)) {
    pdf = do.call(quick.df,as.list(params))
  } else {
    pdf = as.data.frame(params)
  }
  params = as.list(params)
  if (!is.null(cu$yformula_)) {
    xseq = seq(xrange[1],xrange[2], length=xlen)
    params[[cu$xvar]] = xseq
    yseq = eval(cu$yformula_, params)
    if (length(yseq)==1) yseq <- rep(yseq,xlen)
    return(list(x=xseq,y=yseq))    
  }
  if (!is.null(cu$xformula_)) {
    yseq = seq(yrange[1],yrange[2], length=ylen)
    params[[cu$yvar]] = yseq
    xseq = eval(cu$xformula_, params)
    if (length(xseq)==1) xseq <- rep(xseq,ylen)
    return(list(x=xseq,y=yseq))
  }
  
  li = compute.implicit.z(cu, xrange, yrange, params, xlen=xlen,ylen=ylen, z.as.matrix=TRUE)
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
