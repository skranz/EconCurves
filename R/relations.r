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
  
  geom1 = geoms1[[1]]
  geom2 = geoms2[[1]]
  is.geom.right(geom2, geom1)
  is.geom.below(geom2, geom1)
  is.geom.above(geom2, geom1)

  
  geom.to.geom.pos(geom1, geom2)

  geom.to.geom.pos(geom2, geom1)

    
  geom.to.geom.pos(geom1, geom2)

  check.geoms.beside(geoms1[["demand1"]], geoms2[["demand2"]])
}

geom.to.geom.pos = function(new, old, check=c("above","below","left","right")) {
  found = NULL
  
  for (pos in check) {
    fun.name = paste0("is.geom.",pos)
    call = substitute(fun(new,old), list(fun=as.name(fun.name))) 
    if (eval(call)) found = c(found, pos) 
  }
  found
  
}

add.geom.grids = function(geom, dim=c("x","y"), add.min.max=TRUE, overwrite=FALSE) {
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

  
check.geoms.beside = function(old, new) {
  restore.point("check.geom.beside")
  
  if (old$type != new$type) {
    return(ok=NA, ydir = NA, xdir=NA)
  }
  geom.type = old$geom.type
  if (geom.type=="gcurve") {
    return(check.gcurves.beside(old, new))
  }
  return(ok=NA, ydir = NA, xdir=NA)

}

round.to.grid = function(val, step=(end-start)/(length-1), start=range[1], end=range[2], length=101, range=c(0,NA)) {
  round( (val-start) / step)*step + start 
}

gcurve.slopes = function(gcurve) {
  diff(gcurve$y) / diff(gcurve$x)
}

examples.get.geom.monotone.parts = function() {
  rad = seq(0,2*pi, length=100)
  x = sin(rad)
  y = cos(rad)
  plot(x,y)
  geom = list(x=x,y=y)
  segs = get.geom.segments(geom, dim="y")
  colors = c("blue","red","green","orange")
  for (i in seq_along(segs)) {
    seg = segs[[i]]
    points(seg$x,seg$y, col=colors[i])
  }
  
  old = geom
  new = list(x=geom$x+1.7,y=geom$y+1.7 )
  plot(old, ylim=range(c(old$y,new$y)), xlim=range(c(old$x,new$x)))
  points(new, col="blue")
  
  is.geom.below(old=new, new=old)  
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

check.geoms.beside.default = function(old, new, dims = c("x","y")) {
  restore.point("check.gcurves.beside")

  dim = "x"
  for (dim in dims)
  
  
  old.slopes = gcurve.slopes(old)
  new.slopes = gcurve.slopes(new)
  
  # all points of new a right of old
  if (min(new$x) > max(old$x)) {
    xdir = 1
    if ( all(old.slopes<0) & all(new.slopes<0) ) {
      ydir = 1
    } else if ( all(old.slopes>0) & all(new.slopes>0) ) {
      ydir = -1
    } else  {
      ydir = 0
    }
    return(ok=TRUE)
  }
      
  nr = length(old$y)
  nc = length(new$y)
  

  oymat = matrix(old$y, nrow=nr,ncol=nc)
  nymat = matrix(new$y, nrow=nr,ncol=nc, byrow=TRUE)

  oxmat = matrix(old$y, nrow=nr,ncol=nc)
  nxmat = matrix(new$y, nrow=nr,ncol=nc, byrow=TRUE)

    
  ref.shift = gcurve.to.gcurve.shift(gcurve, ref.gcurve)

  point.shift = sign(point.to.gcurve.pos(xy,ref.gcurve))
  if (all(point.shift==ref.shift))
    return(TRUE)
  return(FALSE)

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
    segs = get.geom.segments(geom=geom, dim=dim)
    xseq = seq(geom$xrange[1], geom$xrange[2], length=geom$xlen)
    
    if (length(segs)==1) {
      # nice one-to-one function
      yseq = approx(x = geom$x,y=geom$y,xout=xseq)
      return(list(x=xseq,y=yseq))
    } else {
      
      # deal with backward bending curve
      yseqs = unlist(lapply(segs, function(seg) {
        approx(x=seg$x,y=seg$y, xout=xseq)
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
    
    segs = get.geom.segments(geom=geom, dim=dim)
    yseq = seq(geom$yrange[1], geom$yrange[2], length=geom$ylen)
    if (length(segs)==1) {
      # nice one-to-one function
      xseq = approx(x = geom$y,y=geom$x,xout=yseq)
      return(list(x=xseq,y=yseq))
    } else {
      # deal with backward bending curve
      xseqs = unlist(lapply(segs, function(seg) {
        approx(x=seg$y,y=seg$x, xout=yseq)
      }))
      keep = !is.na(xseqs) 
      yseqs = rep(yseq,times=NROW(segs))[keep]
      xseqs = xseqs[keep]
      ord = order(yseqs,xseqs)
      
      return(list(x=xseqs[ord],y=yseqs[ord]))
    }
  }
  
}