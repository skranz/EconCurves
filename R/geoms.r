#' Convert a list of abstract geometrical objects to a list of geoms
objects.to.geoms = function(objs, params, xrange, yrange,...) {
  geoms = lapply(objs, object.to.geom, params=params, xrange=xrange, yrange=yrange,...)
  geoms
}

#' Convert an abstract geometrical object to a geom
object.to.geom = function(obj,params,xrange, yrange,...) {
  type = obj$type
  if (type=="curve") {
    return(curve.to.geom(obj,params=params,xrange=xrange,yrange=yrange,...))
  } else if (type=="marker") {
    return(marker.to.geom(obj,params=params,xrange=xrange,yrange=yrange,...))
  }
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