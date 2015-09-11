example.plot.pane = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  res = load.model("ThreeEq")
  tt = res$tt
  em = res$em
  init.model(em)
  init.model.scen(em)
  #em$init.var
  em$sim = simulate.model(em)
  sim = em$sim

  
  IS = em$panes[[1]]
  MR_PC = em$panes[[2]]
  
}

draw.line = function(line,lwd.factor=1,...) {
  lines(x=line$x,y=line$y,col=line$color,lty=line$lty,lwd=line$lwd*lwd.factor,...)
}

draw.lines = function(lines,...) {
  lapply(lines, draw.line,...)
}

plot.lines = function(em, lines, pane.names=names(em$panes),...) {
  restore.point("plot.lines")
  line.panes = sapply(lines, function(line) line$pane)
  for (pane in em$panes[pane.names]) {
    plines = lines[line.panes %in% pane$name]
    plot.pane(em=em,pane=pane,lines=plines,...)
  }
}

plot.pane = function(em,pane=1, lines=NULL, alpha=1,main="",mar=c(4,3,1,1), show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8, sim=em$sim, t = 1, scen=em$scen) {
  restore.point("plot.pane")
  
  if (is.numeric(pane) | is.character(pane))
    pane = em$panes[[pane]]
  
  axis = scen$axis
  xrange = as.numeric(axis[[pane$xvar]])
  yrange = as.numeric(axis[[pane$yvar]])
  
  if (is.null(lines)) {
    lines = compute.pane.lines(em=em,pane = pane,t = t,sim = sim)
  }

  par(mar=mar)
  plot.empty.pane(xlim=xrange, ylim=yrange,mar=mar,xlab=pane$xvar,ylab=pane$yvar,main=main, show.grid=show.grid, cex.axis=cex.axis)

  if (length(lines)==0)
    return()
  draw.lines(lines,lwd.factor=lwd.factor)
  
  if (is.null(label.df))
    label.df = find.label.pos(lines,yrange=yrange)
  
  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$line,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)
}



has.pane.all.symbols = function(pane, symbols) {
  restore.point("has.pane.all.symbols")
  pane.symbols = c(pane$curves,names(pane$markers))
  
  lag = symbols[str.starts.with(symbols,"lag_")]
  lag.base = str.right.of(lag, "lag_")
  cur = setdiff(symbols,lag)

  if (!all(lag.base %in% pane.symbols)) return(FALSE)
  if (!all(cur %in% pane.symbols)) return(FALSE)
  return(TRUE)
}


compute.marker.line = function(marker, val, xrange,yrange, lty=2, lwd=1, t=0, pane.name="", level=1) {
  restore.point("computer.marker.line")
  name = marker$name
  pos = val[[name]]
  
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
  
  list(base=name,name=lab,pane=pane.name,t=t,type="marker",lab=lab,axis=marker$axis,x=x,y=y,color=color, lty=lty,lwd=lwd)  
}

compute.curve.line = function(curve, xrange,yrange, val, level=1,lty=1,lwd=2,t=0, pane.name="") {
  restore.point("compute.curve.line")
  cu = curve
  xy = compute.curve.points(cu, xrange, yrange, par=val)
  color = curve.color(cu$color, level)  
  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  name = paste0(cu$name,t)
  if (is.null(curve$label)) {
    lab = name
  } else {
    lab = replace.whiskers(curve$label , list(t=t))
  }
  list(base=cu$name,name=name,pane=pane.name,t=t,type="curve",lab=lab,axis="",x=x,y=y,color=color, lty=lty,lwd=lwd)    
}


compute.pane.lines = function(em, pane, t, sim=em$sim, val=as.list(sim[t,,drop=FALSE]), level=1, symbols=".all", remove.na.lines = TRUE) {
  restore.point("compute.pane.lines")
  axis = em$scen$axis
  xrange = as.numeric(axis[[pane$xvar]])
  yrange = as.numeric(axis[[pane$yvar]])
  
  if (!identical(symbols,".all")) {
    marker.names = intersect(names(pane$markers), symbols)
    curve.names = intersect(pane$curves,symbols)
  } else {
    marker.names = names(pane$markers)
    curve.names = pane$curves
  }
  pane.name = pane$name
  
  # marker
  ma.li = lapply(pane$markers[marker.names], function(marker) {
    compute.marker.line(marker=marker, val = val,xrange = xrange,yrange=yrange, t=t, pane.name=pane.name,level=level)
  })
  
  cu = em$curves[[1]]
  cu.li = lapply(em$curves[curve.names], function(cu) {
    compute.curve.line(cu=cu, xrange = xrange,yrange=yrange,val = val,level=level,t=t, pane.name=pane.name)
  })

  li = c(ma.li, cu.li)
  names(li) = c(marker.names,curve.names)
  
  if (remove.na.lines) {
    is.na.line = sapply(li, function(line) all(is.na(line$x)) | all(is.na(line$y)))
    if (any(is.na.line)) {
      names = lapply(li[is.na.line], function(line) line$name)
      cat("\nLines ", paste0(names, collapse=", ")," can not yet be computed.\n")
      li = li[!is.na.line]
    }
  }
  #bases = sapply(li, function(line) line$base)
  #names(li) = bases
  li
}



