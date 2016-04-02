
example.draw.clicks = function() {

yaml = '
pane:
  curves:
    demand:
      label: D
      eq: y == A - b *p
      color: red
    supply:
      label: S
      eq: p == mc
      color: blue
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,100]
  xmarkers: [y_eq]
  ymarkers: [p_eq]
'
  pane = init.yaml.pane(yaml=yaml)
  values = list(A=100, b=1, mc=20,y_eq=30, p_eq=40)
  pane$geoms = compute.pane.geoms(pane, values=values)

  plot.pane(pane)
  
  clicks = list()
  
  while(length(clicks)<5) {
    click = locator(1)
    clicks[[length(clicks)+1]] = unlist(click)
    draw.clicks(clicks)
  }
}


draw.click = function(click,x=click[["x"]],y=click[["y"]],pch="+", color=grey(0.2), cex=1.2) {
  points(x,y,pch=pch,col=color,cex=cex)
}

draw.clicks = function(clicks,pch="+", color=grey(0.2), cex=1.2, add.line=TRUE) {
  restore.point("draw.clicks")
  
  if ((!is.data.frame(clicks)) & is.list(clicks)) {
    clicks = do.call("rbind",clicks)   
  }
  points(clicks[,1],clicks[,2],pch=pch,col=color,cex=cex)
  lines(clicks[,1],clicks[,2], col=color)
}

click.finds.geom.to.geom.pos = function(click, new, old, check=c("above","below","left","right"), need.all.dir = TRUE,...) {
  gg = geom.to.geom.pos(new, old, check=check)
  cg = point.to.geom.pos(click, old, check=check)
  
  cg = setdiff(cg,"on")
  
  if (need.all.dir) {
    ok = setequal(gg,cg)
  } else {
    ok = length(intersect(gg,cg))>0
  }
  ok
  
}

#' 
click.selects.single.geom = function(click, geoms, on.tol=0.05, single.tol=0.05) {
  restore.point("click.selects.single.geom")
  
  dists = click.dist.to.geoms(click, geoms)
  
  on = which(dists<=on.tol)
  if (length(on)==1) {
    return(list(ok=TRUE, selected=on))
  }
  if (length(on)==0) {
    return(list(ok=FALSE, selected=NULL))
  }
  
  close = which(dists<=single.tol)
  if (length(close)==1) {
    return(list(ok=TRUE, selected=close))
  }
  if (length(close)==0) {
    return(list(ok=FALSE, selected=on))
  }
  return(list(ok=FALSE, selected=close))
  
}

click.dist.to.geoms = function(click, geoms, ...) {
  sapply(geoms, function(geom) {
    point.to.geom.dist(click, geom,...)
  })
}

click.dist.to.geom = function(click, geom,...) {
  point.to.geom.dist(click, geom,...)
} 

is.click.on.geoms = function(click, geoms, on.tol=0.05) {
  sapply(geoms, function(geom) {
    is.point.on.geom(click, geom, on.tol=on.tol)
  })
}

is.click.on.geom = function(click, geom, on.tol=0.05) {
  is.point.on.geom(click, geom, on.tol=on.tol)
}

is.click.on.point = function(click, ref, axis="xy", on.tol=0.05,xrange=pane$xrange, yrange=pane$yrange, pane=NULL) {
  restore.point("has.click.found.point")
  dist = point.to.point.dist(click, ref, axis=axis, xrange=xrange, yrange=yrange, normalize=TRUE)
  
  return(dist<=tol)
}

