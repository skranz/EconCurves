
compute.marker.line = function(marker, val, xrange,yrange, lty=2, lwd=1, t=0, pane.name="", level=1) {
  restore.point("computer.marker.line")
  name = marker$name
  pos = val[name]
  
  if (marker$axis == "x") {
    x = c(pos,pos)
    y = yrange
  } else {
    y = c(pos,pos)
    x = xrange
  }
  color = curve.color(base.color, level,color)
  
  m = marker
  lab = ifelse(str.ends.with(m,"_"),substring(m,1,nchar(m)-1),m)
  lab.t = ifelse(str.starts.with(m,"lag_"),t-1,t)
  lab = ifelse(str.starts.with(m,"lag_"),str.right.of(lab,"lag_"),lab)

  list(base=name,name=lab,pane=pane.name,t=t,type="marker",lab=lab,axis=marker$axis,x=x,y=y,color=color, lty=lty,lwd=lwd)  
}

compute.curve.line = function(curve, xrange,yrange, val, level=1,lty=1,lwd=2, pane.name="") {
  restore.point("draw.curve")
  cu = curve
  xy = compute.curve.points(cu, xrange, yrange, par=val)
  color = curve.color(cu$color, color.level)  
  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  lab = paste0(cu$name,t)
  list(base=name,name=lab,pane=pane.name,t=t,type="curve",lab=lab,axis="",x=x,y=y,color=color, lty=lty,lwd=lwd)    
}


compute.pane.lines = function(em, pane, t, sim=em$sim, val=as.list(sim[t,,drop=FALSE]), level=1) {
  i = 1
  
  pane.name = pane$name
  # marker
  ma.li = lapply(pane$markers, function(marker) {
    compute.marker.line(marker=marker, val = val,xrange = xrange,yrange=yrange, marker=m, t=t, pane.name=pane.name)
  })
  ma.li = do.call(c, ma.li)

  cu.li = lapply(pane$curves, function(cu) {
    compute.curve.line(cu=cu, xrange = xrange,yrange=yrange,val = val,level=level, pane.name=pane.name)
  })

  li = c(ma.li, cu.li)
  bases = sapply(li, function(line) line$base)
  names(li) = bases
  li
}


