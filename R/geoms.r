#' Convert a list of abstract geometrical objects to a list of geoms
objects.to.geoms = function(objs, values, xrange, yrange,...) {
  geoms = lapply(objs, object.to.geom, values=values, xrange=xrange, yrange=yrange,...)
  nulls = sapply(geoms, is.null)

  geoms[!nulls]
}

#' Convert an abstract geometrical object to a geom
object.to.geom = function(obj,values,xrange, yrange,...) {
  type = obj$type
  if (type=="curve") {
    return(curve.to.geom(obj,values=values,xrange=xrange,yrange=yrange,...))
  } else if (type=="marker") {
    return(marker.to.geom(obj,values=values,xrange=xrange,yrange=yrange,...))
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
