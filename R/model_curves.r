# compute.pane.lines
compute.model.pane.lines = function(em, pane, t, sim=em$sim, val=as.list(sim[t,,drop=FALSE]), level=1, symbols=".all", remove.na.lines = TRUE) {
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
    marker.to.geom(marker=marker, val = val,xrange = xrange,yrange=yrange, t=t, pane.name=pane.name,level=level)
  })
  
  cu = em$curves[[1]]
  cu.li = lapply(em$curves[curve.names], function(cu) {
    curve.to.geom(cu=cu, xrange = xrange,yrange=yrange,val = val,level=level,t=t, pane.name=pane.name)
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





# plot.lines
plot.model.lines = function(em, lines, pane.names=names(em$panes),...) {
  restore.point("plot.lines")
  line.panes = sapply(lines, function(line) line$pane)
  for (pane in em$panes[pane.names]) {
    plines = lines[line.panes %in% pane$name]
    plot.pane(em=em,pane=pane,lines=plines,...)
  }
}

# plot.pane
plot.model.pane = function(em,pane=1, lines=NULL, alpha=1,main="",mar=c(4,3,1,1), show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8, sim=em$sim, t = 1, scen=em$scen) {
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
  
  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$label,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)
}

