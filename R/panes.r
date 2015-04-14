
new.pane = function(xvar,yvar,xrange=NULL, yrange=NULL, vp.pos=NULL,curves=NULL, name=paste0(xvar,"-",yvar),par=NULL, fill = rgb(0.9,0.9,0.9)) {
  restore.point("new.pane")
  nlist(xvar=xvar,yvar=yvar,xrange=xrange,yrange=yrange, vp.pos=vp.pos, curves=curves, curves.xy=NULL,fill=fill,par=par)
}

update.curve.on.pane = function(cu, pane, xrange=pane$xrange, yrange=pane$yrange, par=pane$par) {
  cu$xy = compute.points(cu,xrange=xrange, yrange=yrange,par=par)
  
  cu$xrange = range(cu$xy$x)
  cu$yrange = range(cu$xy$y)
  
  cu$is.vertical = diff(cu$xrange) == 0 
  cu$is.horizontal = diff(cu$yrange) == 0
  
  if (!cu$is.vertical)
    cu$approx.fun = approx(cu$xy$x,cu$xy$y, method="linear")
  
}

compute.pane = function(pane, xrange=pane$xrange, yrange=pane$yrange,par=pane$par,...) {
  restore.point("compute.pane")
  pane  = copy.dots.to.pane(pane,par=par,xrange=xrange,yrange=yrange)

  pane$curves = lapply(pane$curves, update.curve.on.pane, pane=pane)
  
  # Find all intersections of curves
  ncu = length(pane$curves)
  inters.li = lapply(setdiff(seq_along(ncu),ncu), function(i) {
    li = lapply((i+1):ncu, i=i, function(j,i) {
      res = curves.xy.intersections(pane$curves[[i]]$xy,pane$curves[[j]]$xy)
      if (length(res$x)==0) return(NULL)
      n=length(res$x)
      quick.df(i=rep(i,n),j=rep(j,n),num=1:n,x=res$x,y=res$y)
    })
    do.call(rbind, li)
  })
  inters = do.call(rbind, inters.li)
  pane$inters = inters
  
  pane
}

draw.pane = function(pane, par=pane$par, xrange=pane$xrange, yrange=pane$yrange, recompute=TRUE,extend.ranges=FALSE,extend.xrange=extend.ranges, extend.yrange=extend.ranges, theme = default.theme(),...) {
  restore.point("draw.pane")
  pane  = copy.dots.to.pane(pane,par=par,xrange=xrange,yrange=yrange)

  pane$par = par
  if (recompute) {
    pane = compute.pane(pane)
  }
  
  if (extend.xrange) {
    ranges = as.vector(sapply(pane$curves, function(cu) range(cu$xy$x, na.rm=TRUE))) 
    xrange = range(c(ranges))
    size = diff(xrange)
    if (size==0) size==1
    xrange = xrange + c(-0.05*size + 0.05*size)
  }
  if (extend.yrange) {
    ranges = as.vector(sapply(pane$curves, function(cu) range(cu$xy$y, na.rm=TRUE))) 
    yrange = range(c(ranges))
    size = diff(yrange)
    if (size==0) size==1
    yrange = yrange + c(-0.05*size + 0.05*size)

  }

  vpp = pane$vp.pos
  pane$vp = viewport(x=vpp$x1, y=vpp$y1, width=vpp$x2-vpp$x1, height=vpp$y2-vpp$y1,
                     just=c("left","bottom"))
    
  #make.pane.viewport(pane)
  pushViewport(pane$vp)
  
  ivp = viewport(x=0,y=0,width=0.95, height=0.9,just=c("left","bottom"),
                 xscale=xrange, yscale=yrange, clip="on")
  pushViewport(ivp)
  if (!is.null(pane$fill))
    grid.rect(x=0,y=0, width=1, height=1,default.units="npc",
              just = c("left","bottom"), gp=gpar(fill=pane$fill, col=pane$fill))

  # draw intersections
  if (NROW(pane$inters)>0) {
    r = 1
    gp = gpar(col=theme$inters.col, lwd=theme$inters.lwd, lty=theme$inters.lty,
              alpha = theme$inters.alpha)
    for (r in 1:NROW(pane$inters)) {
      ix = pane$inters$x[r]; iy = pane$inters$y[r]
      
      grid.lines(x = c(xrange[1],ix),y=c(iy,iy), default.units="native", gp = gp)
      grid.lines(x = c(ix,ix),y=c(yrange[1],iy), default.units="native", gp = gp)
    }
  }
  
  # draw curves
  draw.curves.xy(pane$curves, theme)
  
  popViewport(1)
  
  draw.axis(xvar=pane$xvar,yvar=pane$yvar, theme=theme)
  
  popViewport(1)
  pane
}

copy.dots.to.pane = function(pane,...) {
  args = list(...)
  if (length(args)>0) {
    names = intersect(names(pane),names(args))
    pane[names] = args[names]
  }
  pane  
}

make.pane.viewport = function(pane,just=c("left","bottom"), clip="off",...) {
  vpp = pane$vp.pos
  vp = viewport(x=vpp$x1, y=vpp$y1, width=vpp$x2-vpp$x1, height=vpp$y2-vpp$y1,just=just,xscale=pane$xrange, yscale=pane$yrange,clip=clip,...)
  vp
}


#' Alternative Function to computer axTicks
#' allows no ticks below xlim[1] or ylim[1], respectively
myaxTicks = function(pos=1,lim) {
  ticks = axTicks(pos)
  if (min(ticks)<min(lim)) {
    ticks[1] = lim[1]
  }
  ticks
}

draw.xaxis.ticks <- function (at=pretty(xlim,n=5),y=units(0,"npc"),xlim=c(0,1),n=5) {
  tick.y0 <- y
  tick.y1 <- unit(-0.5, "lines")
  segmentsGrob(unit(at, "native"), tick.y0, unit(at, "native"), 
        tick.y1, name = "ticks")
}
