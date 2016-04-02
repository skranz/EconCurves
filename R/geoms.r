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

objects.to.geoms = function(objs=pane$objs, values=pane$values, xrange=pane$xrange, yrange=pane$yrange,name.prefix="", name.postfix="", label.prefix="", label.postfix="", pane=NULL, color.level=1, ...) {
  geoms = lapply(objs, object.to.geom, values=values, xrange=xrange, yrange=yrange, name.prefix=name.prefix, name.postfix=name.postfix, label.prefix=label.prefix, label.postfix=label.postfix,...)
  
  names(geoms) = paste0(name.prefix, names(objs), name.postfix)
  
  nulls = sapply(geoms, is.null)
 
  geoms[!nulls]
}

#' Convert an abstract geometrical object to a geom
object.to.geom = function(obj,values=pane$values,xrange=pane$xrange, yrange=pane$yrange,xlen=201,ylen=201,pane=NULL, color.level=1,...) {
  restore.point("object.to.geom")
  
  type = obj$type
  if (type=="curve") {
    geom = curve.to.geom(obj,values=values,xrange=xrange,yrange=yrange,xlen=xlen,ylen=ylen,color.level=1,...)
  } else if (type=="marker") {
    geom = marker.to.geom(obj,values=values,xrange=xrange,yrange=yrange,xlen=xlen, ylen=ylen,color.level=1,...)
  }
  if (is.null(geom)) {
    return(NULL)
  }
  
  geom = as.environment(geom)
  geom$values = values
  geom$obj = obj
  geom$xrange=xrange
  geom$yrange=yrange
  geom$xlen = xlen
  geom$ylen = ylen
  
  geom
}

#' Draw a geom
draw.geom = function(geom,...) {
  restore.point("draw.geom")
  
  type = geom$type
  if (type == "curve" | type=="marker") {
    draw.curve(geom,...)
  }
}

#' Draw a list of geoms
draw.geoms = function(geoms,...) {
  for (geom in geoms) draw.geom(geom,...)
}
