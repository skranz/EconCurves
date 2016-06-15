
init.area = function(obj=list(),xy=NULL,fill="#ffff33", name=NULL,pane=NULL) {
  restore.point("init.area")
  if (is.null(obj)) obj = list()
  
  obj = copy.into.null.fields(dest=obj, source=nlist(name,xy,"fill"=fill))

  fields = setdiff(names(obj), c("name","type","xy","xy_","style","class"))
  obj$style = obj[fields]
  
  obj$type = "area"


  xy_ = lapply(obj$xy, function(xy) parse(text=xy))
  xind = seq(1,length(xy_), by=2)
  obj$x_ =xy_[xind]
  obj$y_ =xy_[xind+1]  
  
  
  obj$parnames = unique(unlist(lapply(obj$xy_,find.variables)))
  obj = init.object.extras(obj)
  obj
}


area.to.geom = function(obj,pane, values=pane$values) {
  restore.point("area.to.geom")
  
  x = unlist(lapply(obj$x_, function(x_) eval(x_,values)))
  y = unlist(lapply(obj$y_, function(y_) eval(y_,values)))
  list(type="area", x=x,y=y,xrange=pane$xrange, yrange=pane$yrange)
}

draw.svg.area = function(svg,geom, role=NULL, level=-1, display=NULL) {
  restore.point("draw.svg.area")
  display = init.geom.display(geom, display)

  res = domain.to.range(x=geom$x, y=geom$y, svg=svg)  
  
  points = paste0(res$x,",",res$y, collapse=" ")

  el = svg_tag("polygon", args=nlist(id=geom$id, points=points, style=geom$obj$style, level=level, class="area", display=display),tooltip = geom$tooltip)
  svg_add(svg, el, id=geom$id)
}
