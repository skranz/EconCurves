
init.pane.points = function(pane) {
  restore.point("init.pane.markers")

  if (is.null(pane[["points"]])) {
    pane$points = list()
    return()
  }
  pane$points = lapply(pane$points, function(point) {
    init.point(obj=point,pane = pane)
  })
  invisible(pane$points)
}


init.point = function(obj=list(),xy=NULL,color="#dddd00", name=NULL,pane=NULL) {
  restore.point("init.point")
  if (is.null(obj)) obj = list()
  
  obj = copy.into.null.fields(dest=obj, source=nlist(name,xy,color))
  obj$type = "point"


  if (is.null(obj$xy)) {
    stop("point has not specified xy")
  }
  obj$x_ =  parse(text=obj$xy[1])
  obj$y_ =  parse(text=obj$xy[2])

  obj$parnames = c(find.variables(obj$x_), find.variables(obj$y_))
  obj = init.object.extras(obj)

  obj
}


point.to.geom = function(obj,pane, values=pane$values) {
  restore.point("point.to.geom")
  
  x = eval(obj$x_, values)
  y = eval(obj$y_, values)
  list(type="point", x=x,y=y,xrange=pane$xrange, yrange=pane$yrange)
}
