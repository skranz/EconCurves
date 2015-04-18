example.plot.pane = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  res = load.model("ThreeEq")
  tt = res$tt
  em = res$em
  init.model(em)
  init.model.scen(em)
  #em$init.var
  em$sim = simulate.model(em)
  sim = em$sim

  
  make.periods = function(t) {
    if(t==1) return(1)
    return(c(t-1,t))
  }
  t = 2
  IS = em$panes[[1]]
  MR_PC = em$panes[[2]]
  
  par(mfrow=c(1,2))
  plot.pane(em=em,pane=IS, periods=make.periods(t),show.grid=FALSE)
  plot.pane(em=em,pane=MR_PC, periods=make.periods(t),show.grid=FALSE)
 
}

draw.line = function(line,...) {
  lines(x=line$x,y=line$y,col=line$color,lty=line$lty,lwd=line$lwd,...)
}

draw.lines = function(lines,...) {
  lapply(lines, draw.line,...)
}

plot.lines = function(em, lines,...) {
  restore.point("plot.lines")
  line.panes = sapply(lines, function(line) line$pane)
  for (pane in em$panes) {
    plines = lines[line.panes %in% pane$name]
    plot.pane(em=em,pane=pane,lines=plines,...)
  }
}

plot.pane = function(em,pane, lines, alpha=1,main="",mar=c(3,3,0,0), show.grid=TRUE, label.df=NULL) {
  restore.point("plot.pane")
  axis = em$scen$axis
  xrange = as.numeric(axis[[pane$xvar]])
  yrange = as.numeric(axis[[pane$yvar]])
  
  plot.empty.pane(xlim=xrange, ylim=yrange,mar=mar,xlab=pane$xvar,ylab=pane$yvar,main=main, show.grid=show.grid)

  if (length(lines)==0)
    return()
  draw.lines(lines)
  
  if (is.null(label.df))
    label.df = find.label.pos(lines,yrange=yrange)
  
  boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$line,cex=0.75,bg="white",border=FALSE,xpad=1.1,ypad=1.1)
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