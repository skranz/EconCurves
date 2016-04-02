# Structure of objects
#
# Abstract object:   geom:
#
# curve              gcurve
# marker             gcurve
# point              gpoint
# area               garea


#' plot a single curve in a pane
plot.curve = function(curve=NULL, eq=NULL,pane, main=NULL, label="", ...) {
  eq = substitute(eq)

  restore.point("plot.curve")
  
  if (is.null(curve)) {
    curve = init.curve(name="curve", eq=eq, xvar=pane$xvar, yvar=pane$yvar, label=label, ...)
  }
  geoms = objects.to.geoms(list(curve), pane=pane)
  plot.pane(pane, geoms=geoms,main = main)
}


example.plot.pane = function() {

opc.pane = pane(xvar="u",yvar="dw",xlab="Arbeitslosigkeitsquote (u)", ylab="Relative Lohnsteigerung", xrange=c(0,0.2), yrange=c(-0.1,0.2))
plot.curve(eq = (dw ==-0.1 + 1.1*u), pane=opc.pane, color="blue", main="Hypothese 1")
abline(h=0)
  
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
plot.pane = function(pane,geoms=pane$geoms, xrange=pane$xrange, yrange=pane$yrange, alpha=1,main="",mar=c(4,3,1,1), show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8, 
  xlab= if (is.null(pane$xlab)) pane$xvar else pane$xlab,
  ylab= if (is.null(pane$ylab)) pane$yvar else pane$ylab,
compute=is.null(geoms), params=pane$params, data=pane$data, data.rows=NULL,values=NULL  
  ) {
  restore.point("plot.pane")
  
  if (compute)
    pane$geoms = geoms = compute.pane.geoms(pane=pane,params=params, data=data, data.rows=data.rows,values=values)
  
  par(mar=mar)
  plot.empty.pane(xlim=xrange, ylim=yrange,mar=mar,xlab=xlab,ylab=ylab,main=main, show.grid=show.grid, cex.axis=cex.axis)

  if (length(geoms)==0)
    return()
  draw.geoms(geoms,lwd.factor=lwd.factor)

  if (is.null(label.df))
    label.df = find.label.pos(geoms,yrange=pane$yrange)

  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$label,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)
  
  invisible(pane)

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
compute.pane.geoms = function(pane, values=NULL, objs = pane$objs,xrange=pane$xrange, yrange=pane$yrange,name.prefix=rep("",nr), name.postfix=rep("",nr), label.prefix=rep("",nr), label.postfix=rep("",nr), params=pane$params, data = pane$data, data.rows = NULL, color.level=rep(1,nr), nr=max(1,length(data.rows)), ...) {
  restore.point("computer.pane.geoms")
  
  if (is.null(values)) {
    values = params
  }
  if (is.null(data.rows) | length(data.rows)==1) {
    geoms = objects.to.geoms(objs=objs, values=values, xrange = xrange,yrange=yrange, name.prefix=name.prefix, name.postfix=name.postfix, label.prefix=label.prefix, label.postfix=label.postfix,color.level=color.level,...)
  } else {
    geom.li = lapply(seq_along(data.rows), function(i) {
      objects.to.geoms(objs=objs, values=values, xrange = xrange,yrange=yrange, name.prefix=name.prefix[i], name.postfix=name.postfix[i], label.prefix=label.prefix[i], label.postfix=label.postfix[i],color.level=color.level[i],...)
    })
    geoms = do.call("c", geom.li)
  }
  geoms
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

pane = function(...) as.environment(init.pane(...))

#' Initilize a pane
init.pane = function(pane=list(),name=NULL, xvar=NULL, yvar=NULL, xrange=NULL, yrange=NULL, xlab=NULL, ylab=NULL,  xmarkers=NULL, ymarkers=NULL, geoms=NULL, curves=NULL, init.curves=TRUE, data=NULL, params=NULL) {
  restore.point("init.pane")

  pane = copy.into.null.fields(dest=pane, source=nlist(name,xvar, yvar,xrange,yrange, curves, xmarkers, ymarkers, geoms, xlab, ylab,data, params))


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
init.yaml.pane = function(yaml=NULL, pane=NULL,name=NULL, direct=FALSE) {
  restore.point("init.yaml.pane")

  if (is.null(pane)) {
    li = read.yaml(text=yaml)
    if (!direct) {
      pane = li[[1]]
      if (is.null(pane$name))
        pane$name = names(li)[1]
    } else {
      pane = li
      if (is.null(name)) name = "pane"
      pane$name = name
    }
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



