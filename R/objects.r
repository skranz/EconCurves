
init.pane.objects = function( pane) {
  objects = pane[["objects"]]
  if (is.null(objects)) {
    pane$objects = list()
    return()
  }

  names = names(objects)
  objects = lapply(seq_along(objects), function(i) {
    obj = objects[[i]]
    obj$name = names[[i]]
    init.object(obj,name=names[[i]],pane=pane)
  })
  names(objects) = names
  objects
}


init.object = function(obj, name=obj$name,pane) {
  restore.point("init.object")
  
  type = obj$type
  fun = paste0("init.",type)
  do.call(fun,list(name=name,obj=obj,pane=pane))
  
}
init.object.extras = function(obj) {
  restore.point("init.object.extras")
  
  if (isTRUE(obj$stop)) stop()
  lab = first.non.null(obj$latex, obj$label, obj$name)
  obj$use.latex = !is.null(obj$latex) | is.null(obj$label)
  obj$label.has.whiskers = grepl("{{",lab, fixed=TRUE)
  if (obj$use.latex & !obj$label.has.whiskers) {
    obj$svg_label = latex.to.textspan(lab)
  } else {
    obj$svg_label = lab
  }  
  obj
}