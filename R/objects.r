init.object.extras = function(obj) {
  restore.point("init.object.extras")
  
  lab = first.non.null(obj$latex, obj$label, obj$name)
  obj$label.has.whiskers = grepl("{{",lab, fixed=TRUE)
  if (!is.null(obj$latex) & !obj$label.has.whiskers) {
    obj$svg_label = latex.to.textspan(lab)
  } else {
    obj$svg_label = lab
  }  
  obj
}