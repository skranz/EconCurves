
init.marker = function(obj=list(), name=NULL, var=name, axis = "x", color="#333333", pane=NULL,dashed="5,5") {
  restore.point("init.marker")
  if (is.null(obj)) obj = list()
  
  obj = copy.into.null.fields(dest=obj, source=nlist(name,var,axis,color, dashed))
  obj$type = "marker"
  obj$parnames = var
  
  if (!is.null(obj[["to"]])) {
    if (is.character(obj$to))
      obj$to = parse(text=obj$to)
  }
  if (!is.null(obj[["from"]])) {
    if (is.character(obj$from))
      obj$from = parse(text=obj$from)
  }
  
  obj = init.object.extras(obj)

  obj
}




marker.to.geom = function(obj,pane, values=pane$values) {
  restore.point("marker.to.geom")
  
  pos = values[[obj$var]]
  if (is.null(pos)) {
    msg = paste0("Value of ",obj$var," missing for marker ", obj$name)
    warning(msg); cat(msg)
    return(NULL)
  }
  
  if (!isTRUE(is.finite(pos))) {
    msg = paste0("No finite values in param for marker ", obj$name)
    warning(msg); cat(msg)
    return(NULL)
  }
  
  
  if (obj$axis == "x") {
    x = c(pos,pos)
    y = pane$yrange
    if (!is.null(obj[["to"]])) {
      try(y[2] <- eval.obj.formula(obj[["to"]],values,obj))
    }
    if (!is.null(obj[["from"]])) {
      try(y[1] <- eval.obj.formula(obj[["from"]],values,obj))
    }
    
  } else {
    y = c(pos,pos)
    x = pane$xrange
    if (!is.null(obj[["to"]])) {
      try(x[2] <- eval.obj.formula(obj[["to"]],values,obj))
    }
    if (!is.null(obj[["from"]])) {
      try(x[1] <- eval.obj.formula(obj[["from"]],values,obj))
    }
  }
  list(type="marker", geom.type="gcurve",axis=obj$axis,x=x,y=y,xrange=pane$xrange, yrange=pane$yrange)  
}

eval.obj.formula = function(formula, values, obj) {
  restore.point("eval.obj.formula")
  if (is.character(formula))
    formula = parse(text=formula)
  if (is.numeric(formula))
    return(formula)
  eval(formula,values)
}
