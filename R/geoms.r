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

objects.to.geoms = function(objs=pane$objs,pane, values=pane$values,name.postfix="", data_row=1) {
  restore.point("objects.to.geoms")
  
  geoms = lapply(objs, object.to.geom, values=values, pane=pane, data_row=data_row)
  names(geoms) = paste0(names(objs), name.postfix)
  nulls = sapply(geoms, is.null)
  geoms[!nulls]
}



#' Convert an abstract geometrical object to a geom
object.to.geom = function(obj,pane,values=pane$values, data_row=1) {
  restore.point("object.to.geom")
  type = obj$type
  if (type=="curve") {
    geom = curve.to.geom(obj,values=values,pane=pane)
  } else if (type=="marker") {
    geom = marker.to.geom(obj,values=values,pane=pane)
  } else if (type=="point") {
    geom = point.to.geom(obj,values=values,pane=pane)
  } else {
    fun = paste0(type,".to.geom")
    geom = do.call(fun, list(obj=obj,values=values, pane=pane))
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
  #geom$id = paste0(pane$name,"_",obj$type,"_",obj$name,"_",data_row)
  geom$id = paste0(obj$type,"_",obj$name,"_",data_row)
  labrel = obj[["labrel"]]
  if (!is.null(labrel)) {
    restore.point("dckjdioc wnckw")
    
    n= length(geom$x)-1
    i = c(floor(n * labrel),ceiling(n * labrel)) +1
    geom$labx = mean(geom$x[i])
    geom$laby = mean(geom$y[i])
    
  } else {
    if (!is.null(obj$labx_)) {
      geom$labx = eval(obj$labx_, values)
    } else {
      geom$labx = NA
    }
    if (!is.null(obj$laby_)) {
      geom$laby = eval(obj$laby_, values)
    } else {
      geom$laby = NA
    }
  }
  
  
  geom
}

geom.label = function(geom=NULL,role=NULL, label.replace=NULL) {
  restore.point("geom.label")
  
  #if (geom$obj$name == "pc") stop()
  obj = geom$obj
  label = obj$svg_label

  if (isTRUE(obj$label.has.whiskers)) {
    label = replace.whiskers(label , label.replace)
    if (isTRUE(obj$use.latex)) {
      label = latex.to.textspan(label)
    }
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
