example.plot.pane = function() {

yaml = '
pane:
  curves:
    demand:
      eq: y = A - b *p
      color: red
    supply:
      eq: p = mc
      color: blue
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,100]
'
  pane = init.yaml.pane(yaml=yaml)  
  params = list(A=100, b=1, mc=20)
  pane$lines = compute.pane.lines(pane, params=params)
  
  plot.pane(pane)
}

plot.pane = function(pane,lines=pane$lines, alpha=1,main="",mar=c(4,3,1,1), show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8) {
  
  par(mar=mar)
  plot.empty.pane(xlim=pane$xrange, ylim=pane$yrange,mar=mar,xlab=pane$xvar,ylab=pane$yvar,main=main, show.grid=show.grid, cex.axis=cex.axis)

  if (length(lines)==0)
    return()
  draw.lines(lines,lwd.factor=lwd.factor)
  
  if (is.null(label.df))
    label.df = find.label.pos(lines,yrange=pane$yrange)
  
  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$line,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)

}

compute.pane.lines = function(pane,params, curves=pane$curve,...) {
  lines = lapply(curves,curve.to.line, params=params, xrange=pane$xrange, yrange=pane$yrange,...) 
  lines
}

init.pane = function(name=NULL, xvar=NULL, yvar=NULL, xrange=NULL, yrange=NULL,  xmarkers=NULL, ymarkers=NULL, lines=NULL, curves=NULL, pane=list(), init.curves=TRUE) {
  
  pane = copy.into.null.fields(dest=pane, source=nlist(name,xvar, yvar,xrange,yrange, curves, xmarkers, ymarkers))

  restore.point("init.pane")
  
  if (is.null(pane[["name"]]))
    pane$name = attr(pane,"name")
  
  if (!is.null(pane[["xy"]])) {
    if (is.null(pane$xvar))
      pane$xvar = pane$xy[1]
    if (is.null(pane$yvar))
      pane$yvar = pane$xy[2]
  }
  
  
  xnames = pane$xmarkers; ynames = pane$ymarkers
  pane$xmarkers = lapply(pane$xmarkers, function(marker) {
    marker = list(name=marker)
    marker$axis = "x"
    marker
  })
  names(pane$xmarkers) = xnames
  pane$ymarkers = lapply(pane$ymarkers, function(marker) {
    marker = list(name=marker)
    marker$axis = "y"
    marker
  })
  names(pane$ymarkers) = ynames

  pane$markers = c(pane$xmarkers,pane$ymarkers)
  
  if (!is.null(pane$curves) & init.curves) {
    curve.names = names(pane$curves)
    pane$curves = lapply(seq_along(pane$curves), function(i) {
      init.curve(name=curve.names[i], xvar=pane$xvar, yvar=pane$yvar, curve=pane$curves[[i]])
    })
    names(pane$curves) = curve.names
  }

  pane
}


init.yaml.pane = function(yaml=NULL, pane=NULL) {
  restore.point("init.yaml.pane")
  
  if (is.null(pane)) {
    li = read.yaml(text=yaml)
    pane = li[[1]]
    if (is.null(pane$name))
      pane$name = names(li)[1]
  }
  pane$xrange = unlist(pane$xrange)
  pane$yrange = unlist(pane$yrange)
  
  pane = init.pane(pane=pane)

  pane    
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



