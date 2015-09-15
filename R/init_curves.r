examples.init.curve = function() {
  yaml = '
IS:
  eq: r == A-a*y
  color: red
  xy: [y,r]
'
  
  curve = init.yaml.curve(yaml=yaml)
  line = curve.to.line(curve,xrange=c(0,1),yrange=c(0,200),params=list(A=100,a=1))
}

init.curve = function(name=NULL, eq=NULL, xvar=NULL,yvar=NULL, color=NULL,label=NULL, curve=list(), var.funs=NULL) {
  restore.point("init.curve")
  
  curve = copy.into.null.fields(dest=curve, source=nlist(name,eq,xvar, yvar,color, label))

  
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
  
  curve$eq_ = parse.as.call(text=curve$eq)
  
  # Replace derivatives and variable functions
  if (!is.null(var.funs))
    curve$eq_ = compute.equation.funs(list(curve$eq_),var.funs)[[1]]
  
  res = specialize.curve.formula(curve$eq_, xvar=curve$xvar,yvar=curve$yvar)
  
  curve = c(curve, res)
  curve
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


