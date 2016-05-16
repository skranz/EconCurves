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

objects.to.geoms = function(objs=pane$objs,pane, values=pane$values,name.postfix="") {
  restore.point("objects.to.geoms")
  
  geoms = lapply(objs, object.to.geom, values=values, pane=pane)
  names(geoms) = paste0(names(objs), name.postfix)
  nulls = sapply(geoms, is.null)
  geoms[!nulls]
}



#' Convert an abstract geometrical object to a geom
object.to.geom = function(obj,pane,values=pane$values) {
  restore.point("object.to.geom")
  type = obj$type
  if (type=="curve") {
    geom = curve.to.geom(obj,values=values,pane=pane)
  } else if (type=="marker") {
    geom = marker.to.geom(obj,values=values,pane=pane)
  }
  if (is.null(geom)) {
    return(NULL)
  }
  
  geom = as.environment(geom)
  geom$values = values
  geom$obj = obj
  geom$xrange=pane$xrange
  geom$yrange=pane$yrange
  geom$xlen = pane$xlen
  geom$ylen = pane$ylen
  geom$name = obj$name
  geom
}

geom.label = function(geom=NULL,role=NULL, label.replace=NULL, for.svg=TRUE,label.postfix="", label.prefix="") {
  restore.point("geom.label")
  
  if (for.svg) {
    label = geom$obj$svg_label
  } else {
    label = geom$obj$label
  }

  if (is.null(label)) {
    label = name
  } else {
    if (!is.null(label.replace)) 
      label = replace.whiskers(label , label.replace)
    label = paste0(label.prefix,label,label.postfix)
  }
  label
}

geom.color = function(geom, role) {
  restore.point("geom.color")
  
  obj = geom$obj
  if (is.null(geom[["color"]])) {
    if (!is.null(obj[["color"]])) {
      geom$color =  obj$color
    } else if (!is.null(obj$colors)) {
      geom$color =  curve.color(obj$colors, level=role$color.level)
    }
  }
  geom$color
}