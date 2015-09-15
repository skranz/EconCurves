


shiny.pane.click = function(app=getApp(), es=app$es,pane.name,id, session, value,...) {
  #args = list(...)
  restore.point("shiny.pane.click")
  if (length(value)==0) 
    return()
  restore.point("shiny.pane.click.with.val")
  xy = c(x=value$x,y=value$y)
  cat("\nclick: ")
  print(value)
  
  #cat("Click!")
}


dynry.panes.ui = function(app=getApp(), es=app$es) {
  restore.point("dynry.panes.ui")
  em = es$em
  pane.names = names(em$panes)
  li = lapply(em$panes[pane.names], function(pane) {
    plotId = paste0(pane$name,"_PanePlot")
    clickId = paste0(pane$name,"_PaneClick")
    changeHandler(id=clickId, shiny.pane.click, pane.name=pane$name)
    plotOutput(outputId = plotId,click = clickId, width="250px",height="250px")
  })
  names(li) = NULL
  ui = HTML(html.table(li,ncol=2))
  #ui = do.call(fluidRow, li)
  ui
}

dynry.tell.part.task = function(app=getApp(),es=app$es, t=es$t, step.num=es$step.num) {

  lines = get.dynry.step.lines(es = es,t = t,step = step.num,solved=FALSE,previous.steps = TRUE)
  shiny.plot.lines(em=es$em,lines)
}


shiny.plot.lines = function(lines,app=getApp(), es=app$es, em=es$em,pane.names=names(em$panes),...) {
  restore.point("shiny.plot.lines")
  line.panes = sapply(lines, function(line) line$pane)
  lapply(em$panes[pane.names], function(pane) {
    plines = lines[line.panes %in% pane$name]
    plotId = paste0(pane$name,"_PanePlot")
    setPlot(id = plotId, plot.pane(em=em,pane=pane,lines=plines,lwd.factor=2,label.cex=1,cex.axis=1))
  })
  setText("plotCounter",sample(1:1000,1))
}

