examples.init.curve = function() {
  yaml = '
IS:
  eq: r == A-a*y
  color: red
  xy: [y,r]
'
  
  curve = init.yaml.curve(yaml=yaml)
  geom = curve.to.geom(curve,xrange=c(0,1),yrange=c(0,200),values=list(A=100,a=1))
}

init.curve = function(name=NULL, eq=NULL, xvar=NULL,yvar=NULL, color=NULL,label=NULL, curve=list(), var.funs=NULL,...) {
  restore.point("init.curve")
  
  curve = copy.into.null.fields(dest=curve, source=nlist(name,eq,xvar, yvar,color, label))

  if (is.character(curve$eq)) {
    curve$eq_ = parse.as.call(text=curve$eq)
  } else {
    curve$eq_ = curve$eq = strip.parentheses(curve$eq)
  }

  
  if (is.null(curve$name)) {
    curve$name = attr(curve,"name")
  }
  
  if (is.null(curve$label))
    curve$label=curve$name
  if (!is.null(curve["xy"])) {
    if (is.null(curve$xvar))
      curve$xvar = curve$xy[1]
    if (is.null(curve$xvar))
      curve$yvar = curve$xy[2]
  }

  check.curve(curve)
  
  
  # Replace derivatives and variable functions
  if (!is.null(var.funs))
    curve$eq_ = compute.equation.funs(list(curve$eq_),var.funs)[[1]]
  
  res = specialize.curve.formula(curve$eq_, xvar=curve$xvar,yvar=curve$yvar)
  
  curve = c(curve, res)
  curve$type = "curve"
  curve = init.object.extras(curve)
  curve
}

init.curves = function(curves,...) {
  curve.names = names(curves)
  curves = lapply(seq_along(curves), function(i) {
    init.curve(name=curve.names[i],..., curve=curves[[i]])
  })
  names(curves) = curve.names
  curves
}

init.yaml.curve = function(yaml=NULL, curve=NULL, var.funs=NULL) {
  restore.point("init.yaml.curve")
  
  if (is.null(curve)) {
    li = read.yaml(text=yaml)
    curve = li[[1]]
    if (is.null(curve$name))
      curve$name = names(li)[1]
  }
  
  init.curve(curve=curve, var.funs=var.funs)
}

specialize.curve.formula = function(eq, xvar, yvar, level=NULL, solve.symbolic = TRUE) {
  restore.point("specizalize.curve.formula")
  formula_ = eq
  lhs_ = get.lhs(formula_)
  lhs = deparse1(lhs_)
  rhs_ = get.rhs(formula_)
  
  vl = find.variables(lhs_)
  vr = find.variables(rhs_)

  yformula_ = xformula_ = NULL

  curve.vars = c(vl, vr)
  is.vertical = ! yvar  %in% curve.vars
  is.horizontal = ! xvar  %in% curve.vars

  # y variable is alone on lhs
  if (identical(lhs,yvar) & (! yvar %in% vr)) {
    yformula_ = substitute(rhs, list(rhs=rhs_))

  } else if (solve.symbolic) {
    res = sym.solve.eq(eq,yvar, simplify=TRUE)
    if (res$solved)
      yformula_ = res$eq[[3]]
    
  }

  # x variable is alone on lhs
  if (identical(lhs,xvar) & (! xvar %in% vr)) {
    xformula_ = substitute(rhs, list(rhs=rhs_))
  } else if (solve.symbolic) {
    res = sym.solve.eq(eq,xvar, simplify=TRUE)
    if (res$solved)
      xformula_ = res$eq[[3]]
  }
  
  # implicit formula
  implicit_ = substitute(lhs-(rhs), list(lhs=lhs_,rhs=rhs_))
  
  curve = nlist(eq_=eq,yformula_, xformula_,implicit_,is.horizontal, is.vertical,xvar,yvar)
  slope_ = compute.curve.slope(curve)
  slope.vars = find.variables(slope_)
  is.linear = (!xvar %in% slope.vars) & (! yvar %in% slope.vars) 
  
  ret = nlist(xformula_, yformula_, implicit_,slope_, is.vertical, is.horizontal, is.linear, curve.vars, slope.vars, parnames = setdiff(curve.vars,c(xvar,yvar)))
  ret
}

# compute symbolically a curve's slope
compute.curve.slope = function(curve) {
  restore.point("compute.curve.slope")
  
  if (isTRUE(curve$is.horizontal)) {
    slope = 0
  } else if (isTRUE(curve$is.vertical)) {
    slope = Inf
  } else if (!is.null(curve$yformula_)) {
    slope = Deriv::Deriv(curve$yformula_, curve$xvar)
  } else if (!is.null(curve$xformula_)) {
    slope = substitute(1 / (invslope))
  } else {
    dFdx =  Deriv::Deriv(curve$implicit_, curve$xvar)
    dFdy =  Deriv::Deriv(curve$implicit_, curve$yvar)
    slope = Deriv::Simplify(substitute(-dFdx/dFdy))
  }
  slope  
}


# compute.curve.gcurve
curve.to.geom = function(curve,pane, values=pane$values, xlen=pane$xlen, ylen=pane$ylen,xrange=pane$xrange,yrange=pane$yrange, ...) {
  restore.point("curve.to.geom")
  
  
  cu = curve
  xy = compute.curve.points(cu,xrange, pane$yrange, values=values,xlen=xlen,ylen=ylen)
  
  if (!isTRUE((any(is.finite(xy$x+xy$y))))) {
    warning(paste0("No finite values for curve ", curve$name))
    return(NULL)
  }

  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  list(type="curve",axis="",x=x,y=y,xrange=xrange,yrange=yrange)    
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

