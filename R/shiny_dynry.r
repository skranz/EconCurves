
examples.shiny.dynry = function() {
  set.restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  es = load.story("ThreeEq_G_langfristig")
  es = load.story("SimpleLabor3EqStory.yaml")
  init.story(es)
  es$t = 1
  app = shinyStoryApp(es)
  runEventsApp(app,launch.browser = rstudio::viewer)

  em = es$em
  sim = em$sim
  dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)

}

shinyStoryApp = function(es,...) {
  restore.point("shinyStoryApp")
  
  library(shinyEvents)  
  library(shinyAce)
  library(shinyBS)
  
  app = eventsApp()
  app$es = es
  app$allow.edit = get.ec()$allow.edit
  ui = story.ui()
  ui = fluidPage(title = es$storyId,ui)

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    app$es = es
  }, app=app)
  
  app$ui = ui
  dynry.tell.part.task()
  app
}




dynry.ui = function(app=getApp(), es=app$es) {
  restore.point("dynry.ui")
  if (app$allow.edit) {
    editBtn = actionButton("stEditBtn","Edit")
    refreshBtn = actionButton("stRefreshBtn","Refresh")
  } else {
    editBtn = refreshBtn = NULL
  }
  ui = fluidRow(
    column(4,
     fluidRow(
        actionButton("stPrevBtn","<"),
        actionButton("stNextBtn",">"),
        actionButton("stForwardBtn",">>"),
        actionButton("stExitBtn","Exit"),
        editBtn, refreshBtn
      ),
      uiOutput("tellUI"),
      uiOutput("answerUI")
    ),
    column(8,
      dynry.panes.ui(),
      dygraphOutput("dygraph",height="150px")
      #uiOutput("panesUI")
    )
  ) 
  #setPlot("testPlot",{plot(runif(100),runif(100))})
  #setUI(id = "panesUI",dynry.panes.ui())
  buttonHandler("stNextBtn", dynry.next.btn.click,if.handler.exists = "skip")
  buttonHandler("stForwardBtn", dynry.forward.btn.click,if.handler.exists = "skip")
  buttonHandler("stPrevBtn", dynry.prev.btn.click,if.handler.exists = "skip")
  buttonHandler("stExitBtn", exit.to.main,if.handler.exists = "skip")
  buttonHandler("stEditBtn", edit.dynry.click,if.handler.exists = "skip")
  buttonHandler("stRefreshBtn", refresh.dynry.click,if.handler.exists = "skip")
  

  ui
}


edit.dynry.click = function(app=getApp(), es=app$es,...) {
  restore.point("edit.dynry.click")
  cat("Edit Story...")
  
}

dynry.wait.for.answer = function(app=getApp(), es=app$es) {
  restore.point("stNextBtnClicked")
  t=es$t
  step.num=es$step.num
  part = get.dynry.part(es=es,t=t, step.num=step.num)
  if (length(part$task)==0) {
    es$wait.for.answer = FALSE
    return(FALSE)
  }
  es$attempts = 0
  es$wait.for.answer = TRUE
  return(TRUE)
}
  
dynry.process.click.answer = function(app=getApp(), es=app$es,xy, pane.name,...) {
  restore.point("dynry.check.click.answer")
  res = check.click.answer(es = es,xy = xy,pane.name = pane.name)

  # correct
  if (res) {
    es$wait.for.answer = FALSE
    dynry.tell.part.sol()
  } else {
    es$attempts = es$attempts+1
    setUI(id = "answerUI",p(paste0("Not correct. (", es$attempts, " attempts.)")))
  } 
}  

shiny.pane.click = function(app=getApp(), es=app$es,pane.name,id, session, value,...) {
  #args = list(...)
  restore.point("shiny.pane.click")
  if (length(value)==0) 
    return()
  restore.point("shiny.pane.click.with.val")
  xy = c(x=value$x,y=value$y)
  cat("\nclick: ")
  print(value)
  
  # outside a task just continue
  if (!is.true(es$wait.for.answer)) {
    dynry.next.btn.click()
  } else {
    dynry.process.click.answer(app = app,es=es,xy = xy,pane.name = pane.name)
  }
  #cat("Click!")
}


refresh.dynry.click = function(app=getApp(), es=app$es,...) {
  cat("Refresh dynry...")
  t = es$t; step.num = es$step.num
  id = es$storyId
  restore.point("refresh.dynry.click")

  # reload and init story  
  es = load.story(id)
  app$es = es
  init.story(es)
  
  dynry.tell.part.task(t = t,step.num = step.num)
  
}


dynry.next.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("stNextBtnClicked")
  #cat("stNextBtn was clicked...")
  
  if (isTRUE(es$wait.for.answer)) {
    setUI(id = "answerUI",p(paste0("You have not yet answered the question.")))
    return()
  }
  
  res = dynry.next(es=es)
  if (res$end) return()
  dynry.tell.part.task()
  
  if (dynry.wait.for.answer(es=es))
    return()
  
  dynry.tell.part.sol()
}

dynry.forward.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("dynry.forward.btn.click")
  
  dynry.forward(es, update.es=TRUE)
  
  dynry.tell.part.task()
  if (dynry.wait.for.answer(es=es))
    return()
  dynry.tell.part.sol()
}


dynry.prev.btn.click = function(app=getApp(), es=app$es,...)  {
  restore.point("stPrevBtnClicked")
  res = dynry.prev(es=es,update.es = TRUE)
  if (res$start) return()
  dynry.tell.part.task()
  dynry.tell.part.sol()
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
  restore.point("dynry.tell.part.task")
  es$t = t; es$step.num = step.num
  
  part = get.dynry.part(es,t,step.num)
  html = compile.story.txt(c(part$tell,part$ask), em=es$em, t=t,out = "html")
  html = paste0("<h4>", t,".", step.num,"</h4>", html)
  setUI(id = "tellUI",HTML(html))
  setUI(id = "answerUI",HTML(""))
  
  lines = get.dynry.step.lines(es = es,t = t,step = step.num,solved=FALSE,previous.steps = TRUE)
  shiny.plot.lines(em=es$em,lines)
}

dynry.tell.part.sol = function(app=getApp(),es=app$es, t=es$t, step.num=es$step.num) {
  restore.point("dynry.tell.part.sol")
  
  es$t = t; es$step.num = step.num
  
  part = get.dynry.part(es,t,step.num)
  html = compile.story.txt(c(part$success), em=es$em, t=t,out = "html")
  setUI(id = "answerUI",HTML(html))

  lines = get.dynry.step.lines(es = es,t = t,step = step.num,solved=TRUE,previous.steps = TRUE)
  shiny.plot.lines(em=es$em,lines=lines)
  
  dygraph =dynry.step.dyplot(es=es,t=t,step = step.num,solved = TRUE,vars = es$timelineVars)
  app$output$dygraph = renderDygraph(dygraph)

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

