#' Convert a list of abstract geometrical objects to a list of geoms
#' 
#' @objs the list of objects
#' @values a list of values used to evaluate the object formulas to compute the geoms
#' @values objs by default all objects of the pane, but alternatively, other objects can be provided
#' @xrange the x-axis range on which geoms shall be computed (default is pane$xrange)
#' @yrange the y-axis range on which geoms shall be computed (default is pane$yrange)
#' @name.prefix a prefix added to object names (useful if we have several geoms per object computed from different values)
#' @name.postfix a postfix added to object names (useful if we have several geoms per object computed from different values)
#' @label.prefix a prefix added to object label (useful if we have several geoms per object computed from different values)
#' @label.postfix a postfix added to object label (useful if we have several geoms per object computed from different values)

objects.to.geoms = function(objs, values, xrange, yrange,name.prefix="", name.postfix="", label.prefix="", label.postfix="", ...) {
  geoms = lapply(objs, object.to.geom, values=values, xrange=xrange, yrange=yrange, name.prefix=name.prefix, name.postfix=name.postfix, label.prefix=label.prefix, label.postfix=label.postfix,...)
  
  names(geoms) = paste0(name.prefix, names(objs), name.postfix)
  
  nulls = sapply(geoms, is.null)
 
  geoms[!nulls]
}

#' Convert an abstract geometrical object to a geom
object.to.geom = function(obj,values,xrange, yrange,...) {
  type = obj$type
  if (type=="curve") {
    geom = curve.to.geom(obj,values=values,xrange=xrange,yrange=yrange,...)
  } else if (type=="marker") {
    geom = marker.to.geom(obj,values=values,xrange=xrange,yrange=yrange,...)
  }
  geom$values = values
  geom$obj = obj
  geom
}

#' Draw a geom
draw.geom = function(geom,...) {
  geom.type = geom$geom.type
  if (geom.type == "line") {
    draw.line(geom,...)
  }
}

#' Draw a list of geoms
draw.geoms = function(geoms,...) {
  for (geom in geoms) draw.geom(geom,...)
}
