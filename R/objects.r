init.object.extras = function(obj) {
  if (!is.null(obj$latex)) {
    obj$svg_label = latex.to.textspan(obj$latex)
  } else if (!is.null(obj$label)) {
    obj$svg_label = obj$label
  } else {
    obj$label = obj$name
  }
  
  
  obj
}