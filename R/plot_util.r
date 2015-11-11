examples.plot.empty.pane = function() {
  xlim = c(0,10)
  ylim = c(-2,5)
  plot.empty.pane(xlim=xlim,ylim=ylim)
}


plot.empty.pane = function(xlim,ylim,mar=c(4,4,4,1),mgp=c(2,2,0),xlab="",ylab="",main="",x.ticks = pretty(xlim,n.ticks,),y.ticks=pretty(ylim,n.ticks),n.ticks=10, zero = c(min(xlim),min(ylim)), show.grid=TRUE, cex.axis=0.8, ...) {
  restore.point("plot.empty.pane")
  #par(mar=mar,mpg=mpg)
  plot(x=mean(xlim),y=mean(ylim),xlim=xlim,ylim=ylim,axes=FALSE,frame.plot=FALSE, type="n",xlab=xlab,ylab=ylab, main=main,mgp=mgp,mar=mar)
  #plot.arrow.axes(xlim=xlim,ylim=ylim,axes=TRUE)

  if (show.grid) {
    draw.pane.grid(xlim = xlim,ylim=ylim,x.ticks=x.ticks,y.ticks = y.ticks)
  }
  # draw grid

#  axis(side=1,pos = zero[2],at=x.ticks,cex.axis=0.8)
#  axis(side=2,pos = zero[1],at=y.ticks, cex.axis=0.8)
  axis(side=1,at=x.ticks,cex.axis=cex.axis)
  axis(side=2,at=y.ticks, cex.axis=cex.axis)
  
  #grid(nx=n.ticks,ny=n.ticks)
}

draw.pane.grid = function(xlim,ylim, col=grey(.8),x.ticks = pretty(xlim,n.ticks,),y.ticks=pretty(ylim,n.ticks),n.ticks=10) {
  
  segments(x0=x.ticks, y0=min(ylim),y1=max(ylim), col=col)
  segments(y0=y.ticks, x0=min(xlim),x1=max(xlim), col=col)
  
}

examples.plot.arrow.axes = function() {
  plot.arrow.axes(x=1:10,y=runif(10))

}

plot.arrow.axes = function(x=NULL,y=NULL,xlim=range(x),ylim=range(y),axes = !TRUE,  frame.plot=FALSE, zero = c(xlim[1],ylim[1]), rel.extend.x = 0,rel.extend.y = 0,code=2, do.plot=!is.null(x),...) {
  
  
  if (do.plot) {            
    plot(x=x,y=y,...,xlim=xlim,ylim=ylim,axes=FALSE,frame.plot=frame.plot)
  }

  if (axes) {
    axis(side=1,pos=zero[2])
    axis(side=2,pos=zero[1])
  }
  
  x.end = xlim[2]+diff(xlim)*rel.extend.x
  y.end = ylim[2]+diff(xlim)*rel.extend.y
  
  
  arrows(zero[1], zero[2], x1 = x.end, length = 0.05, angle = 30, code = code,
       col = par("fg"), lty = par("lty"), lwd = par("lwd"))
  arrows(zero[1], zero[2], y1 = y.end, length = 0.05, angle = 30, code = code,
       col = par("fg"), lty = par("lty"), lwd = par("lwd"))
       
}




barlegend <- function (col, labels=NULL, line.labels=NULL, x = NULL , y = NULL ,orient="vert",...) 
{
   if (is.null(x)) {
     if (orient=="vert") {
        x=c(0.02,0.08)
      } else {
        x=c(0.1,0.9)
      }
   }
   if (is.null(y)) {
     if (orient=="vert") {
        y=c(0.1,0.9)
      } else {
        y=c(0.05,0.15)
      }
   }
   usr <- par("usr")
   xlim = usr[1:2]
   ylim = usr[3:4]
   xpos = (x) * diff(xlim) +xlim[1]
   ypos = (y) * diff(ylim) +ylim[1]
   
   if (orient=="vert") {
    y.seq = seq(ypos[1],ypos[2],length=length(col)+1)
    for (i in 1:length(col)) {
      rect(xpos[1],y.seq[i],xpos[2],y.seq[i+1],col=col[i])
    }
    if (!is.null(line.labels)) {
      if (length(line.labels) == length(col)-1) {
        text(xpos[2],y.seq[2:(length(y.seq)-1)],labels=line.labels,pos=4,...)
      } else {
        text(xpos[2],y.seq,labels=line.labels,pos=4,...)
      }
    }
   }
   
}


# Function which creates breaks, centered around a certain value (usually used here around 0).
# If center.discrete = TRUE, we will only have an epsilon intervall around this value, which might be useful
# e.g. when we have a mixed discrete-continous variable with a lot of observations being directly 0.
# center.breaks are useful for freqareaplot

center.breaks = function(x,center=mean(x),size=abs(diff(pretty(x,n)[1:2])),n=20,
                         center.discrete=FALSE,breakpoints.to.abs.lower=(center==0 & center.discrete),center.width = 1e-6) {
  x = x[is.finite(x)]
  eps = 1e-6 #Minimal epsilon which the function hist can distinguish
  if (!center.discrete) {
    low = ceiling(abs(center-0.5*size-min(x))/size)
    up = ceiling(abs(max(x)-center-0.5*size)/size)
    ret <- seq(center-low*size-0.5*size,center+up*size+0.5*size,by = size)
  } else {
    low = ceiling(abs(center-min(x))/size)
    up = ceiling(abs(max(x)-center)/size)
    ls = rs = NULL
    if (low>center)
      ls = seq(center-low*size,center-size,by=size)
    if (up>center)
      rs = seq(center+size,center+up*size,by=size)
    ret <- c(ls,-center.width,center.width,rs)
    # e.g. if we have breaks (-2,-1,0,1,2) 
    # normaly a value of -1 counts for the interval (-2,-1) where as a value 1 counts for (0,1)
    # using the epsiolon adjustment also -1 counts for the intervall (-1,0)
    # Only senseful for center.discrete = 0
    if (breakpoints.to.abs.lower) {
      ret[ret<(-eps)] = ret[ret<(-eps)]-eps
    }
  }
  ret
}


contour.colors = function(num.color=200,col="darkredgreen") {
  if (col=="redgreen") {
    cols = c(rainbow(num.color,start=5/6, end = 1/3),grey(0.6)) # From Magenta into green
  } else if (col=="darkredgreen") {
    cols = c(rainbow(num.color,start=5/6, end = 1/3,v = (0.3+0.7*(1:num.color)/num.color)),grey(0)) # From Magenta into green
  } else if (col=="blackwhite") {
    cols = c(grey((0:(num.color-1))/(num.color-1)),"blue") # From Black into white
  }
}


# Red-Yellow-Green Scheme for negative and positive numbers
ryg.color <- function(nred=10,ngreen=nred, have.yellow=TRUE) {
  # rainbow 5/6 = red 1/12 = yellow 2/6 = green
  num.color = nred
  rcol = rainbow(num.color,start=5/6,end=0,v = (0.3+0.7*(1:num.color)/num.color))
  num.color = ngreen
  gcol = rainbow(num.color,start=1/6,end=2/6,v = (0.3+0.7*(num.color:1)/num.color))
  
  if (have.yellow) {
    c(rcol,"yellow",gcol)
  } else {
    c(rcol,gcol)
  }
}

# from plotrix package
boxed.labels <- function (x, y = NA, labels, bg = ifelse(match(par("bg"), "transparent", 
    0), "white", par("bg")), border = TRUE, xpad = 1.2, ypad = 1.2, 
    srt = 0, cex = 1, adj = 0.5, xlog = FALSE, ylog = FALSE, 
    ...) 
{
  restore.point("boxed.labels")
  

  oldpars <- par(c("cex", "xpd"))
  par(cex = cex, xpd = TRUE)
  if (is.na(y) && is.list(x)) {
      y <- unlist(x[[2]])
      x <- unlist(x[[1]])
  }
  box.adj <- adj + (xpad - 1) * cex * (0.5 - adj)
  if (srt == 90 || srt == 270) {
      bheights <- strwidth(labels)
      theights <- bheights * (1 - box.adj)
      bheights <- bheights * box.adj
      lwidths <- rwidths <- strheight(labels) * 0.5
  } else {
      lwidths <- strwidth(labels)
      rwidths <- lwidths * (1 - box.adj)
      lwidths <- lwidths * box.adj
      bheights <- theights <- strheight(labels) * 0.5
  }
  args <- list(x = x, y = y, labels = labels, srt = srt, adj = adj, 
      col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) < 
          350, "white", "black"))
  args <- modifyList(args, list(...))
  if (xlog) {
      xpad <- xpad * 2
      xr <- exp(log(x) - lwidths * xpad)
      xl <- exp(log(x) + lwidths * xpad)
  } else {
      xr <- x - lwidths * xpad
      xl <- x + lwidths * xpad
  }
  if (ylog) {
      ypad <- ypad * 2
      yb <- exp(log(y) - bheights * ypad)
      yt <- exp(log(y) + theights * ypad)
  } else {
      yb <- y - bheights * ypad
      yt <- y + theights * ypad
  }
  rect(xr, yb, xl, yt, col = bg, border = border)
  do.call("text", args)
  par(cex = oldpars)
}




alpha.color <- function(color,alpha=1)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  trans = alpha*255

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}
