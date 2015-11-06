# Structure of objects
#
# Abstract object:   geom:
#
# Curve              curve
# Marker             marker
# Point              point
# Area               area



example.plot.pane = function() {

yaml = '
pane:
  curves:
    demand:
      label: D{{idD}}
      eq: y == A - b *p
      color: red
    supply:
      label: S{{idS}}
      eq: p == mc
      color: blue
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,150]
  xmarkers: [y_eq]
  ymarkers: [p_eq]
'
  pane = init.yaml.pane(yaml=yaml)
  values1 = list(A=100, b=1, mc=20,y_eq=30, p_eq=40, idD=1,idS="")
  geoms1 = compute.pane.geoms(pane, values=values1, name.postfix="1")

  values2 = list(A=130, b=1, mc=20,y_eq=30, p_eq=40, idD=2,idS="")
  geoms2 = compute.pane.geoms(pane, values=values2, name.postfix="2", color.level = 2)

  pane$geoms = c(geoms1, geoms2["demand2"])
  #  pane$geoms = compute.pane.geoms(pane, values=values, name.postfix="2")

  check.geoms.beside(geoms1[["demand1"]], geoms2[["demand2"]])
  
  plot.pane(pane)
  
  res = locator(1)
  
}

#' Plot a pane
plot.pane = function(pane,geoms=pane$geoms, xrange=pane$xrange, yrange=pane$yrange, alpha=1,main="",mar=c(4,3,1,1), show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8) {

  par(mar=mar)
  plot.empty.pane(xlim=xrange, ylim=yrange,mar=mar,xlab=pane$xvar,ylab=pane$yvar,main=main, show.grid=show.grid, cex.axis=cex.axis)

  if (length(geoms)==0)
    return()
  draw.geoms(geoms,lwd.factor=lwd.factor)

  if (is.null(label.df))
    label.df = find.label.pos(geoms,yrange=pane$yrange)

  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$label,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)

}

#' Compute concrete geoms for all objects of a pane
#' 
#' @pane the pane object
#' @values a list of values used to evaluate the object formulas to compute the geoms
#' @values objs by default all objects of the pane, but alternatively, other objects can be provided
#' @xrange the x-axis range on which geoms shall be computed (default is pane$xrange)
#' @yrange the y-axis range on which geoms shall be computed (default is pane$yrange)
#' @name.prefix a prefix added to object names (useful if we have several geoms per object computed from different values)
#' @name.postfix a postfix added to object names (useful if we have several geoms per object computed from different values)
#' @label.prefix a prefix added to object label (useful if we have several geoms per object computed from different values)
#' @label.postfix a postfix added to object label (useful if we have several geoms per object computed from different values)
compute.pane.geoms = function(pane, values, objs = pane$objs,xrange=pane$xrange, yrange=pane$yrange,name.prefix="", name.postfix="", label.prefix="", label.postfix="", ...) {
  geoms = objects.to.geoms(objs=objs, values=values, xrange = xrange,yrange=yrange, name.prefix=name.prefix, name.postfix=name.postfix, label.prefix=label.prefix, label.postfix=label.postfix,...)
}



create.yaml.pane.markers = function(pane) {
  restore.point("create.yaml.pane.markers")

  xnames = pane$xmarkers; ynames = pane$ymarkers
  pane$xmarkers = lapply(pane$xmarkers, function(marker) {
    init.marker(name=marker, axis="x")
  })
  names(pane$xmarkers) = xnames
  pane$ymarkers = lapply(pane$ymarkers, function(marker) {
    init.marker(name=marker, axis="y")
  })
  names(pane$ymarkers) = ynames
  markers = c(pane$xmarkers,pane$ymarkers)
  markers
}

#' Initilize a pane
init.pane = function(pane=list(),name=NULL, xvar=NULL, yvar=NULL, xrange=NULL, yrange=NULL,  xmarkers=NULL, ymarkers=NULL, lines=NULL, curves=NULL, init.curves=TRUE) {

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

  pane$markers= create.yaml.pane.markers(pane)

  if (!is.null(pane$curves) & init.curves) {
    curve.names = names(pane$curves)
    pane$curves = lapply(seq_along(pane$curves), function(i) {
      init.curve(name=curve.names[i], xvar=pane$xvar, yvar=pane$yvar, curve=pane$curves[[i]])
    })
    names(pane$curves) = curve.names
  }

  pane$objs = c(pane$curves, pane$markers)

  pane
}

#' Initialize a pane specified with yaml code
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



