example.ggvis.plot.pane = function() {
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

  pane = em$panes[[2]]
  gg = ggvis.plot.pane(em=em,periods=c(1:4),opacity=0.2, pane=pane)
  gg
}

ggvis.plot.pane <- function(em,periods=1, pane=em$panes[[1]], lwd=3, opacity=1,gg=NULL) {
  restore.point("ggvis.plot.pane")
  pane.name = pane$name
  
  axis = em$scen$axis
  xrange = as.numeric(axis[[pane$xvar]])
  yrange = as.numeric(axis[[pane$yvar]])
  
  curve.names = pane$curves 
  curves = em$curves[curve.names]
 
   
  cu.li = lapply(periods, function(t) {
    if (t == max(periods)) {
      color.level = 1
    } else {
      color.level = 2
    }
    val = as.list(em$sim[t,])
    cu.df = compute.pane.curves.df(curves=curves, xrange=xrange, yrange=yrange, val=val,color.level = color.level)
    cu.df$t = t
    
    cu.df
  })
  cu.df = bind_rows(cu.li)

  library(ggvis)
  if (is.null(gg)) {
    gg = ggvis(data = cu.df,x=~x, y=~y,stroke := ~color, strokeWidth := lwd,opacity:=0.9, fill = ~t) %>%
     add_axis("x", title = pane$xvar) %>% 
     add_axis("y", title = pane$yvar) %>%
     layer_lines()
  } else {
    gg = gg %>% 
       layer_lines(data = cu.df)
  }
  
  gg 
}