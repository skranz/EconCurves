# when is a geom new below a geom old
# i) no common x range
#    max(new$y) < max(old$y) & min(new$y) < min(old$y)
# ii) a common x range
#    for all x in the common x range new$y(x) < old$y(x)
#    and the max, min conditions hold weakly 
# 

examples.geom.relations = function() {
  yaml = '
  pane:
    curves:
      demand:
        label: D{{idD}}
        eq: y == A - b *p
        color: red
      supply:
        label: S{{idS}}
        eq: p == mc
        color: blue
    xy: [y,p]
    xrange: [0,100]
    yrange: [0,150]
    xmarkers: [y_eq]
    ymarkers: [p_eq]
  '
  pane = init.yaml.pane(yaml=yaml)
  values1 = list(A=100, b=1, mc=20,y_eq=30, p_eq=40, idD=1,idS="")
  geoms1 = compute.pane.geoms(pane, values=values1, name.postfix="1")

  values2 = list(A=130, b=1, mc=20,y_eq=30, p_eq=40, idD=2,idS="")
  geoms2 = compute.pane.geoms(pane, values=values2, name.postfix="2", color.level = 2)

  pane$geoms = c(geoms1, geoms2["demand2"])
  #  pane$geoms = compute.pane.geoms(pane, values=values, name.postfix="2")

  plot.pane(pane)

  
    
  click = locator(n=1,type="p")
  click.selects.single.geom(click, pane$geoms)
  
  geom1 = geoms1[[1]]
  geom2 = geoms2[[1]]

  
  pane$geoms = geoms1
  plot.pane(pane)
  click = locator(n=1,type="p")  
  click.finds.geom.to.geom.pos(click, geom2, geom1)
  
  
  is.geom.right(geom2, geom1)
  is.geom.below(geom2, geom1)
  is.geom.above(geom2, geom1)

  
  geom.to.geom.pos(geom1, geom2)

  geom.to.geom.pos(geom2, geom1)

  point.to.geom.pos(c(90,30), geom2)
  point.to.geom.pos(c(90,30), geom1)
  
  point.to.geom.pos(c(90,30), geoms1[[3]])
  point.to.geom.pos(c(30,30), geoms1[[3]])

  geom.to.geom.pos(geom1, geom2)
  geom.to.geom.pos(geom1, geoms1[[3]])

}

#' Find relative position (above, below, left, right) of a point to a geom
point.to.geom.pos = function(xy, geom,check=c("above","below","left","right","on"), tol.on=0.01) {
  restore.point("point.to.geom.pos")
  
  found = NULL
  
  for (pos in check) {
    fun.name = paste0("is.point.",pos,".geom")
    call = substitute(fun(xy=xy,geom=geom, tol.on=tol.on), list(fun=as.name(fun.name))) 
    if (eval(call)) found = c(found, pos) 
  }
  found
  
  
}

#' Find relative position (above, below, left, right) of a geom to a geom
#' 
#' If the geoms intersect non of the attributes holds true, i.e.
#' the geom new must be stricly above old
geom.to.geom.pos = function(new, old, check=c("above","below","left","right")) {
  found = NULL
  for (pos in check) {
    fun.name = paste0("is.geom.",pos)
    call = substitute(fun(new,old), list(fun=as.name(fun.name))) 
    if (eval(call)) found = c(found, pos) 
  }
  found
}

is.point.below.geom = function(xy, geom,...) {
  restore.point("is.point.below.geom")
  x = xy[[1]]; y = xy[[2]]
  
  x = round.to.grid(x,range=geom$xrange, length = geom$xlen)
  geom = add.geom.grids(geom,dim="x")   

  ind = which(geom$xgr.min$x==x)
  
  isTRUE(y < geom$xgr.min$y[ind])
}

is.point.above.geom = function(xy, geom,...) {
  restore.point("is.point.above.geom")
  x = xy[[1]]; y = xy[[2]]

  x = round.to.grid(x,range=geom$xrange, length = geom$xlen)
  geom = add.geom.grids(geom,dim="x")   

  ind = which(geom$xgr.max$x==x)
  
  isTRUE(y > geom$xgr.max$y[ind])
}

is.point.left.geom = function(xy, geom,...) {
  restore.point("is.point.above.geom")
  x = xy[[1]]; y = xy[[2]]

  y = round.to.grid(y,range=geom$yrange, length = geom$ylen)
  geom = add.geom.grids(geom,dim="y")   

  ind = which(geom$ygr.min$y==y)
  
  isTRUE(x < geom$ygr.min$x[ind])
}


is.point.right.geom = function(xy, geom,...) {
  restore.point("is.point.above.geom")
  x = xy[[1]]; y = xy[[2]]

  y = round.to.grid(y,range=geom$yrange, length = geom$ylen)
  geom = add.geom.grids(geom,dim="y")   

  ind = which(geom$ygr.max$y==y)
  
  isTRUE(x > geom$ygr.max$x[ind])
}

point.to.point.dist = function(xy,ref, axis="xy",normalize=TRUE, xrange=pane$xrange, yrange=pane$yrange,pane=NULL, ...) {
  restore.point("point.dist.to.geom")
  x = xy[[1]]; y = xy[[2]]
  rx= ref[[1]]; ry = ref[[2]]
  
  if (normalize) {
    xs = diff(pane$xrange)
    ys = diff(pane$yrange)
  } else {
    xs = ys = 1
  }

  if (axis=="x") {
    dist = abs(x-rx) / xs
  } else if (axis=="y") {
    dist = abs(y-ry) / ys
  } else if (axis=="xy") {
    dist = sqrt(((x-rx)/xs)^2+ ((y-ry)/ys)^2)
  }
  dist
}


point.to.geom.dist = function(xy, geom, axis="xy",normalize=TRUE, xrange=geom$xrange, yrange=geom$yrange, ...) {
  restore.point("point.dist.to.geom")
  x = xy[[1]]; y = xy[[2]]

  if (normalize) {
    xs = diff(xrange)
    ys = diff(yrange)
  } else {
    xs = ys = 1
  }

  geom = add.geom.grids(geom,dim=c("x", "y"))   
  gx=c(geom$x, geom$xgr$x, geom$ygr$x)
  gy=c(geom$y, geom$xgr$y, geom$ygr$y)
      
  if (axis=="x") {
    dist = min(abs(x-gx) / xs, na.rm = TRUE)
  } else if (axis=="y") {
    dist = min(abs(y-gy) / ys, na.rm = TRUE)
  } else if (axis=="xy") {
    dist = min(sqrt(((x-gx)/xs)^2+ ((y-gy)/ys)^2), na.rm=TRUE)
  }
  dist
}


is.point.on.geom = function(xy, geom, on.tol=0.01,...) {
  restore.point("is.point.on.geom")
  dist = point.to.geom.dist(xy,geom, normalize=TRUE)
  if (length(dist)==0) return(FALSE)
  dist <= on.tol
}


is.geom.below = function(new, old) {
  restore.point("is.geom.below")

  nyr = range(new$y)
  oyr = range(old$y)

  # if min or max is above, so new is not below old  
  if (any(nyr>oyr)) return(FALSE)

  new = add.geom.grids(new,dim="x")
  old = add.geom.grids(old,dim="x")

  below = new$xgr.max$y < old$xgr.min$y
  any(is.true(below)) & !any(is.false(below))
}

is.geom.above = function(new, old) {
  restore.point("is.geom.above")

  nyr = range(new$y)
  oyr = range(old$y)

  if (any(nyr<oyr)) return(FALSE)

  new = add.geom.grids(new,dim="x")
  old = add.geom.grids(old,dim="x")

  above = new$xgr.min$y > old$xgr.max$y
  any(is.true(above)) & !any(is.false(above))
}

is.geom.left = function(new, old) {
  restore.point("is.geom.left")

  nxr = range(new$x)
  oxr = range(old$x)

  if (any(nxr>oxr)) return(FALSE)

  new = add.geom.grids(new,dim="y")
  old = add.geom.grids(old,dim="y")

  left = new$ygr.max$x < old$ygr.min$x
  any(is.true(left)) & !any(is.false(left))
}

is.geom.right = function(new, old) {
  restore.point("is.geom.right")

  nxr = range(new$x)
  oxr = range(old$x)

  if (any(nxr>oxr)) return(FALSE)

  new = add.geom.grids(new,dim="y")
  old = add.geom.grids(old,dim="y")

  right = new$ygr.min$x > old$ygr.max$x
  any(is.true(right)) & !any(is.false(right))
}


round.to.grid = function(val, step=(end-start)/(length-1), start=range[1], end=range[2], length=101, range=c(0,NA)) {
  round( (val-start) / step)*step + start 
}


get.geom.segments = function(geom, dim="x") {
  restore.point("get.geom.segments")
  
  cdim = if (dim=="x") "y" else "x"
  sig = sign(diff(geom[[dim]]))
  
  swing = which(diff(sig)!=0)
  
  # All x bewegen sich in gleiche Richtung
  if (length(swing)==0) {
    return(list(list(x=geom$x,y=geom$y)))
  }
  inds = c(1,swing+1, length(geom[[dim]]))
  res = lapply(1:(length(inds)-1), function(i) {
    rows = inds[i]:inds[i+1]
    list(x=geom$x[rows],y=geom$y[rows])
  })
  res 
}

geom.max.grid = function(geom, grid=NULL, dim="x", dir="max") {
  geom.min.grid(geom,grid,dim,dir)
}

geom.min.grid = function(geom, grid=NULL, dim="x", dir="min") {
  restore.point("geom.min.grid")
  
  if (dim=="x") {
    if (is.null(grid))
      grid = geom$xgr
    odim = "y"
    sign = if (dir=="min") 1 else -1
    
    xseq = seq(geom$xrange[1], geom$xrange[2], length=geom$xlen)

    ord = order(grid$x, sign*grid$y)
    x = c(grid$x[ord],xseq)
    y = c(grid$y[ord],rep(NA,length(xseq)))
    
    dupl = duplicated(x)
    nx = x[!dupl]
    ny = y[!dupl]
    nord = order(nx)
    return(list(x=nx[nord],y=ny[nord]))
  }
  
  if (dim=="y") {
    if (is.null(grid))
      grid = geom$ygr
    odim = "x"
    sign = if (dir=="min") 1 else -1
    
    yseq = seq(geom$yrange[1], geom$yrange[2], length=geom$ylen)

    ord = order(grid$y, sign*grid$x)
    y = c(grid$y[ord],yseq)
    x = c(grid$x[ord],rep(NA,length(yseq)))
    
    dupl = duplicated(y)
    nx = x[!dupl]
    ny = y[!dupl]
    nord = order(ny)
    return(list(x=nx[nord],y=ny[nord]))
    
  }
}

compute.geom.grid = function(geom, dim="x", use.object=TRUE) {
  restore.point("compute.geom.grid")
  
  if (!is.null(geom[["obj"]]) & use.object) {
    if (geom$obj$type=="curve") {
      return(compute.curve.grid(cu=geom$obj,geom=geom,dim=dim))
    }
  }
  if (dim=="x") {
    if (length(unique(geom$x))==1) {
      restore.point("nfbdhfbhrbdufbur")
      
      return(list(
        x=round.to.grid(geom$x,length=geom$xlen, range=geom$xrange),
        y=geom$y
      ))
    }
    
    segs = get.geom.segments(geom=geom, dim=dim)
    xseq = seq(geom$xrange[1], geom$xrange[2], length=geom$xlen)
    
    if (length(segs)==1) {
      # nice one-to-one function
      yseq = approx(x = geom$x,y=geom$y,xout=xseq)$y
      return(list(x=xseq,y=yseq))
    } else {
      
      # deal with backward bending curve
      yseqs = unlist(lapply(segs, function(seg) {
        approx(x=seg$x,y=seg$y, xout=xseq)$y
      }))
      keep = !is.na(yseqs) 
      xseqs = rep(xseq,times=NROW(segs))[keep]
      yseqs = yseqs[keep]
      ord = order(xseqs,yseqs)
      
      return(list(x=xseqs[ord],y=yseqs[ord]))
        
    }
  }
  if (dim=="y") {
    restore.point("compute.grid.y")
    if (length(unique(geom$y))==1) {
     restore.point("nfbdhfbhrbdufefef3bur")
      
      return(list(
        x=geom$x,
        y=round.to.grid(geom$y,length=geom$ylen, range=geom$yrange)
      ))
    }
    
    segs = get.geom.segments(geom=geom, dim=dim)
    yseq = seq(geom$yrange[1], geom$yrange[2], length=geom$ylen)
    if (length(segs)==1) {
      xseq = approx(x = geom$y,y=geom$x,xout=yseq)$y
      # nice one-to-one function
      return(list(x=xseq,y=yseq))
    } else {
      # deal with backward bending curve
      xseqs = unlist(lapply(segs, function(seg) {
        approx(x=seg$y,y=seg$x, xout=yseq)$y
      }))
      keep = !is.na(xseqs) 
      yseqs = rep(yseq,times=NROW(segs))[keep]
      xseqs = xseqs[keep]
      ord = order(yseqs,xseqs)
      
      return(list(x=xseqs[ord],y=yseqs[ord]))
    }
  }
  
}

add.geom.grids = function(geom, dim=c("x","y"), add.min.max=TRUE, overwrite=FALSE) {
  restore.point("add.geom.grids")
  
  if ("x" %in% dim) {
    if (is.null(geom[["xgr"]]) |  overwrite) {
      geom$xgr = compute.geom.grid(geom, dim="x")
    }
    if (add.min.max) {
      if (is.null(geom[["xgr.max"]])  | overwrite) {
        geom$xgr.max = geom.max.grid(geom,geom$xgr,dim = "x")
      }
      if (is.null(geom[["xgr.min"]])  | overwrite) {
        geom$xgr.min = geom.min.grid(geom,geom$xgr,dim = "x")
      }
    }  
  }
  if ("y" %in% dim) {
    if (is.null(geom[["ygr"]]) |  overwrite) {
      geom$ygr = compute.geom.grid(geom, dim="y")
    }
    if (add.min.max) {
      if (is.null(geom[["ygr.max"]])  | overwrite) {
        geom$ygr.max = geom.max.grid(geom,geom$ygr,dim = "y")
      }
      if (is.null(geom[["ygr.min"]])  | overwrite) {
        geom$ygr.min = geom.min.grid(geom,geom$ygr,dim = "y")
      }
    }  
  }
  geom  
}

