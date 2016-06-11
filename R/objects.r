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