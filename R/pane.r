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
  pane$params = list(A=100, b=1, mc=20,y_eq=30, p_eq=40, idD=1,idS="")
  make.pane.data(pane)

  compute.pane.geoms(pane)
  plot.pane(pane)
  
  res = locator(1)
  
}

#' Plot a pane
plot.pane = function(pane, show = pane$show, hide=pane[["hide"]], xrange=pane$xrange, yrange=pane$yrange, alpha=1,main="",mar=NULL, show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8, 
  xlab= if (is.null(pane$xlab)) pane$xvar else pane$xlab,
  ylab= if (is.null(pane$ylab)) pane$yvar else pane$ylab,
compute.geoms=TRUE, params = pane$params, data=pane$data, data_rows=1
  ) {
  restore.point("plot.pane")
  
  pane$data = data
  pane$params = params
  pane$yrange = yrange
  pane$xrange = xrange
  data_rows = unlist(data_rows)

  
  missing.cols = check.for.missing.data.cols(pane,pane$data, show=show)

  if (compute.geoms)
    compute.pane.geoms(pane=pane,data_rows=data_rows)
  
  if (is.null(mar)) {
    mar = c(4,3,1,1)
    if (is.null(main)) mar[3] = 0
  }
  par(mar=mar)
  plot.empty.pane(xlim=xrange, ylim=yrange,mar=mar,xlab=xlab,ylab=ylab,main=main, show.grid=show.grid, cex.axis=cex.axis)

  
  geoms = NULL
  i = 1
  if (identical(show,".all")) {
    show = names(pane$objs)
  }  
  for (i in seq_along(data_rows)) {
    row = data_rows[i]
    if (is.list(show)) {
      cur.show = show[[i]]
    } else {
      cur.show = show
    }
    if (is.list(hide)) {
      cur.show = setdiff(cur.show, hide[[i]])
    } else {
      cur.show = setdiff(cur.show, hide)  
    }
    cur.geoms = pane$geoms.li[[row]][cur.show]   

    geoms = c(geoms, cur.geoms)
  }
  if (is.null(geoms)) {
    cat("\nNo geoms drawn...")
    return()
  }
  
  draw.geoms(geoms,lwd.factor=lwd.factor)
  
  if (is.null(label.df))
    label.df = find.label.pos(geoms,xrange = pane$xrange, yrange=pane$yrange)

  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$label,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)

  pane$geoms = geoms  
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
compute.pane.geoms = function(pane, data_rows = 1, objs = pane$objs, overwrite = TRUE) {
  restore.point("computer.pane.geoms")

  if (isTRUE(pane$data_xrange))
    pane$xrange = compute.pane.range.from.data(pane=pane,ax="x", data_rows=data_rows)

  if (isTRUE(pane$data_yrange))
    pane$yrange = compute.pane.range.from.data(pane,"y", data_rows)

  if (is.null(names(data_rows)))
    names(data_rows) = data_rows
    
  for (i in seq_along(data_rows)) {
    r = data_rows[[i]]
    if (!overwrite) {
      if (!is.null(pane$geoms.li[[r]])) next
    }
    values = c(as.list(pane$data[r,]), list(.row=r, .role=names(data_rows)[i]))
    
    pane$geoms.li[[r]] = objects.to.geoms(objs=objs, values=values, pane=pane, data_row=r)
  }
}

compute.pane.range.from.data = function(pane, ax="x", data_rows) {
  restore.point("compute.pane.range.from.data")
  data = pane$data

  vars = names(data)
    
  var = pane[[paste0(ax,"var")]]
  minvar = paste0(var,"_min")
  maxvar = paste0(var,"_max")
  addrange = pane[[paste0("add_",ax,"range")]]
  if (!minvar %in% vars) {
    stop(paste0("The pane ", pane$name, " has not specified the ",ax,"range and neither does its data contain the variable ", minvar, ", which could be used to compute the range."))
  }
  if (!maxvar %in% vars) {
    stop(paste0("The pane ", pane$name, " has not specified the ",ax,"range and neither does its data contain the variable ", maxvar, ", which could be used to compute the range."))
  }
  
  
  start = min(data[[minvar]][data_rows])
  end = max(data[[maxvar]][data_rows])
  start = start - (end-start)*addrange[1]
  end =  end + (end-start)*addrange[2]
  
  c(start,end)
  
}

does.geom.change.with.data.row = function(data_rows,obj=geom$obj, data=pane$data, pane=NULL, geom=NULL) {
  parnames =  
  len = sapply(obj$parnames, function(par) {
    length(unique(data[[par]][data_rows]))
  })
  any(len>0)
}

do.geoms.differ.across.rows = function(pane, data_rows, objs=pane$objs) {
  data = pane$data
  lapply(seq_along(objs), function(oi) {
    obj = objs[[oi]]
    parnames = obj$parnames 
    
        
  })
  for (oi in seq_along(objs)) {
  }
  
  
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
old.compute.pane.geoms = function(pane, objs = pane$objs,xrange=pane$xrange, yrange=pane$yrange,name.prefix=rep("",nr), name.postfix=rep("",nr), label.prefix=rep("",nr), label.postfix=rep("",nr), params=pane$params, data = pane$data, data_rows = NULL, color.level=rep(1,nr), nr=max(1,length(data_rows)), compute.data = FALSE, ...) {
  restore.point("computer.pane.geoms")
  
  
  
  if (is.null(values)) {
    values = params
  }
  if (is.null(data_rows) | length(data_rows)==1) {
    geoms = objects.to.geoms(objs=objs, values=values, xrange = xrange,yrange=yrange, name.prefix=name.prefix, name.postfix=name.postfix, label.prefix=label.prefix, label.postfix=label.postfix,color.level=color.level,...)
  } else {
    geoms.li = lapply(seq_along(data_rows), function(i) {
      objects.to.geoms(objs=objs, values=values, xrange = xrange,yrange=yrange, name.prefix=name.prefix[i], name.postfix=name.postfix[i], label.prefix=label.prefix[i], label.postfix=label.postfix[i],color.level=color.level[i],...)
    })
    geoms = do.call("c", geom.li)
  }
  geoms
}



pane = function(...) as.environment(init.pane(...))

#' Initilize a pane
init.pane = function(pane=list(),name=NULL, xvar=NULL, yvar=NULL, xrange=NULL, yrange=NULL, xaxis=list(), yaxis=list(),  xmarkers=NULL, ymarkers=NULL, geoms.li=NULL, curves=NULL, init.curves=TRUE, data=NULL, params=NULL, datavar=NULL, use_dataenv_directly = FALSE, data_roles =NULL, show=".all", hide=NULL, xlen=201,ylen=201, org.width = 420, org.height=300, margins=pane$margins, init.data=FALSE, dataenv=parent.frame(), data_xrange=NA , data_yrange=NA, add_xrange = c(0, 0), add_yrange=c(0, 0.1))  {
  restore.point("init.pane")

  
  pane = as.list(pane)
  
  pane$org.li = pane
  
  pane = copy.into.null.fields(dest=pane, source=nlist(name,xvar, yvar,xrange,yrange, curves, xmarkers, ymarkers, geoms.li, xaxis, yaxis, params, datavar, use_dataenv_directly, data_roles, show, hide,xlen,ylen, add_xrange, add_yrange, data_xrange, data_yrange))

  
  if (is.null(pane$xaxis)) pane$xaxis = list()
  if (is.null(pane$yaxis)) pane$yaxis = list()
  
  pane$xaxis = copy.into.null.fields(dest=pane$xaxis, source=list(show.ticks=first.non.null(pane$show.ticks,TRUE),label=pane$xlab))
  
  pane$yaxis = copy.into.null.fields(dest=pane$yaxis, source=list(show.ticks=first.non.null(pane$show.ticks,TRUE),label=pane$ylab))
 
  pane$xaxis = copy.into.null.fields(dest=pane$xaxis, source=list(show.tick.labels=first.non.null(pane$show.tick.labels,pane$xaxis$show.ticks)))
 
  pane$yaxis = copy.into.null.fields(dest=pane$yaxis, source=list(show.tick.labels=first.non.null(pane$show.tick.labels,pane$yaxis$show.ticks)))
  
  if (is.na(pane$data_xrange)) pane$data_xrange = is.null(pane$xrange)
  if (is.na(pane$data_yrange)) pane$data_yrange = is.null(pane$yrange)
  
  if (is.null(margins)) {
    margins = c(bottom=60,left=60, top=20, right=20)
    if (!isTRUE(pane$yaxis$show.ticks)) 
      margins["left"] = 40
    if (!isTRUE(pane$xaxis$show.ticks)) 
      margins["bottom"] = 40
    
  }
  pane$margins=margins
  
  pane = as.environment(pane)

  if (is.null(pane[["name"]]))
    pane$name = attr(pane,"name")

  if (!is.null(pane[["xy"]])) {
    if (is.null(pane$xvar))
      pane$xvar = pane$xy[1]
    if (is.null(pane$yvar))
      pane$yvar = pane$xy[2]
  }


  if (!is.null(pane$curves) & init.curves) {
    curve.names = names(pane$curves)
    pane$curves = lapply(seq_along(pane$curves), function(i) {
      init.curve(name=curve.names[i], xvar=pane$xvar, yvar=pane$yvar, curve=pane$curves[[i]])
    })
    names(pane$curves) = curve.names
  }
  pane$points = init.pane.points(pane)

  pane$markers= init.pane.markers(pane)
  
  pane$objects = init.pane.objects(pane)
  
  pane$objs = c(pane$curves, pane$markers, pane$points, pane$objects)
  
  pane$parnames =unique(unlist(lapply(pane$objs, function(obj) obj$parnames)))
  
  if (!is.null(pane$vars)) {
    library(EconModels)
    make.pane.model(pane)
  }
  
  pane$org.width = org.width
  pane$org.height = org.height
  pane$margins = margins

  if (init.data)  
    pane$data = make.pane.data(pane=pane, dataenv=dataenv)
  pane
}

update.pane.objs = function(pane, curves=NULL, points=NULL,xmarkers = NULL, ymarkers=NULL, objects=NULL) {
  restore.point("update.pane.objs")
  
  if (is.null(curves) & is.null(points) & is.null(xmarkers) & is.null(ymarkers) & is.null(objects)) {
    return(pane)
  }
  
  if (!is.null(curves)) {
    if (is.null(pane$curves)) pane$curves = list()
    curve.names = names(curves)
    
    pane$curves[curve.names] = lapply(seq_along(curves), function(i) {
      restore.point("dfhdfufhriu")
      
      if (curve.names[i] %in% names(pane$org.li$curves)) {
        curve = copy.into.nested.list(pane$org.li$curves[curve.names[i]],new = curves[i])[[1]]
      } else {
        curve = curves[[i]]
      }
      init.curve(name=curve.names[i], xvar=pane$xvar, yvar=pane$yvar, curve=curve)
    })
  }

  if (!is.null(points)) {
    restore.point("update.pane.points")
    
    if (is.null(pane[["points"]]))
      pane$points =list()
    
    pane$points[names(points)] = lapply(names(points), function(name) {
      init.point(obj=points[[name]],name=name,pane = pane)
    })
  }
  
  names_markers = c(names(xmarkers),names(ymarkers))
  xmarkers = lapply(names(xmarkers), function(name) {
    init.marker(xmarkers[[name]],name=name, axis="x", pane=pane)
  })
  ymarkers = lapply(names(ymarkers), function(name) {
    init.marker(ymarkers[[name]],name=name, axis="y", pane=pane)
  })
  markers = c(xmarkers,ymarkers)
  names(markers)= names_markers
  pane$markers[names(markers)] = markers
  
  if (!is.null(objects)) {
    restore.point("update.pane.objects")
    
    if (is.null(pane[["objects"]]))
      pane$objects =list()
    
    pane$objects[names(objects)] = lapply(names(objects), function(name) {
      init.object(obj=objects[[name]],name=name,pane = pane)
    })
  }
  
  
  pane$objs = c(pane$curves, pane$markers, pane$points, pane$objects)
  pane$parnames =unique(unlist(lapply(pane$objs, function(obj) obj$parnames)))

  pane
    
}

#' Initialize a pane specified with yaml code
init.yaml.pane = function(yaml=NULL, pane=NULL,name=NULL, direct=FALSE, init.data=FALSE) {
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
  pane$data_rows = unlist(pane$data_rows)
  
  pane = init.pane(pane=pane, dataenv = parent.frame())

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


make.pane.data = function(pane=NULL,params=pane$params, priority_params = pane$priority_params, data=pane$data, datavar = pane$datavar, dataenv = pane$dataenv, use_dataenv_directly = pane$use_dataenv_directly, parnames = pane$parnames, warn.missing.param, compute.model=TRUE) {
  restore.point("make.pane.data")
  
  # fetch datavar from dataenv
  if (is.null(data) & !is.null(dataenv) & !is.null(datavar)) {
    data = dataenv[[datavar]]
  }
  if (is.null(data) | NROW(data)==0) data = list()
  
  # copy params into data
  cols = setdiff(names(params),names(data))
  data[cols] = params[cols]
  
  # copy values from dataenv into data
  if (isTRUE(use_dataenv_directly) & (!is.null(dataenv))) {
    cols = setdiff(ls(dataenv),names(data))
    for (col in cols) {
      data[[col]] = dataenv[[col]]
    }
  }
  
  # copy priority_params
  cols = names(priority_params)
  data[cols] = priority_params
  
  data = as.data.frame(data,stringsAsFactors = FALSE)
  
  if (isTRUE(pane$has.model) & compute.model & NROW(data)>0) {
    var.mat = pane$em$sim.fun(data)
    data = cbind(data, as.data.frame(var.mat))
  }
  data = as_data_frame(data)
  
  if (!is.null(pane)) pane$data = data
  invisible(data)
}

check.for.missing.data.cols = function(pane,data, show.warning=TRUE, show=".all") {
  restore.point("check.for.missing.data.cols")
  
  parnames = pane$parnames
  if (!identical(show,".all")) {
    parnames = intersect(parnames, unlist(show))
  }

  missing.cols = setdiff(pane$parnames, names(data))
  if (length(missing.cols)>0 & show.warning) {
    msg = paste0("\nThe required data columns ", paste0(missing.cols, collapse = ", "), " are missing.")
    message("Warning: ",msg)
  }
  missing.cols
}


