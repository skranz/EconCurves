
examples.shiny.story = function() {

  set.restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  #es = load.story("ThreeEq_G_langfristig")
  #es = load.story("ThreeEqFixedM_G_langfristig")
  es = load.story("IS_LM_PC_lag_G_kurzfristig")
  
  
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
  ui = story.ui()
  ui = fluidPage(title = es$storyId,ui)

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    app$es = es
  }, app=app)
  
  app$ui = ui
  shiny.tell.step.task()
  app
}

story.ui = function(app=getApp(), es=app$es) {
  restore.point("story.ui")
  ui = fluidRow(
    column(4,
     fluidRow(
        actionButton("stNextBtn","Next"),
        actionButton("stForwardBtn",">>"),
        actionButton("stPrevBtn","Prev"),
        actionButton("stExitBtn","Exit")
      ),
      uiOutput("tellUI"),
      uiOutput("answerUI")
    ),
    column(8,
      story.panes.ui(),
      dygraphOutput("dygraph",height="150px")
      #uiOutput("panesUI")
    )
  ) 
  #setPlot("testPlot",{plot(runif(100),runif(100))})
  #setUI(id = "panesUI",story.panes.ui())
  buttonHandler("stNextBtn", story.next.btn.click)
  buttonHandler("stForwardBtn", story.forward.btn.click)
  buttonHandler("stPrevBtn", story.prev.btn.click)
  buttonHandler("stExitBtn", exit.to.main)
  

  ui
}

story.wait.for.answer = function(app=getApp(), es=app$es) {
  restore.point("stNextBtnClicked")
  t=es$t
  step.num=es$step.num
  period = get.story.period(es=es,t=t)
  step = period$steps[[step.num]]
  if (length(step$task)==0) {
    es$wait.for.answer = FALSE
    return(FALSE)
  }
  
  es$attempts = 0
  es$wait.for.answer = TRUE
  return(TRUE)
}
  
story.process.click.answer = function(app=getApp(), es=app$es,xy, pane.name,...) {
  restore.point("story.check.click.answer")
  res = check.click.answer(es = es,xy = xy,pane.name = pane.name)

  # correct
  if (res) {
    es$wait.for.answer = FALSE
    shiny.tell.step.sol()
  } else {
    es$attempts = es$attempts+1
    setUI(id = "answerUI",p(paste0("Not correct. (", es$attempts, " attempts.)")))
  } 
}  

shiny.pane.click = function(app=getApp(), es=app$es,pane.name,value,...) {
  #args = list(...)
  restore.point("shiny.pane.click")
  xy = c(x=value$x,y=value$y)
  
  # outside a task just continue
  if (!is.true(es$wait.for.answer)) {
    story.next.btn.click()
  } else {
    story.process.click.answer(app = app,es=es,xy = xy,pane.name = pane.name)
  }
  #cat("Click!")
}

story.next.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("stNextBtnClicked")
  #cat("stNextBtn was clicked...")
  
  if (isTRUE(es$wait.for.answer)) {
    setUI(id = "answerUI",p(paste0("You have not yet answered the question.")))
    return()
  }
  
  res = story.next.step(es=es)
  if (res$end) return()
  shiny.tell.step.task()
  
  if (story.wait.for.answer(es=es))
    return()
  
  shiny.tell.step.sol()
}


story.forward.btn.click = function(app=getApp(), es=app$es,...) {

  period = get.story.period(es)
  period.num = period$period.num

  restore.point("story.forward.btn.click")

  if (es$t < es$T) {
    es$t = get.period.t(es,period.num+1)
    es$step.num = 1
  } else {
    if (es$step.num >= length(period$steps))
      return()
    es$step.num = length(period$steps)
  }

  shiny.tell.step.task()
  if (story.wait.for.answer(es=es))
    return()
  shiny.tell.step.sol()
}


story.prev.btn.click = function(app=getApp(), es=app$es,...)  {
  restore.point("stPrevBtnClicked")
  res = story.prev.step(es=es,update.es = TRUE)
  if (res$start) return()
  shiny.tell.step.task()
  shiny.tell.step.sol()
}

story.panes.ui = function(app=getApp(), es=app$es) {
  restore.point("story.panes.ui")
  em = es$em
  pane.names = names(em$panes)
  li = lapply(em$panes[pane.names], function(pane) {
    plotId = paste0(pane$name,"_PanePlot")
    clickId = paste0(pane$name,"_PaneClick")
    changeHandler(id=clickId, shiny.pane.click, pane.name=pane$name)
    plotOutput(outputId = plotId,clickId = clickId, width="250px",height="250px")
  })
  names(li) = NULL
  ui = HTML(html.table(li,ncol=2))
  #ui = do.call(fluidRow, li)
  ui
}

story.prev.step = function(es, t=es$t, step.num=es$step.num, update.es=TRUE) {
  restore.point("story.prev.step")  

  start = FALSE
  if (step.num > 1) {
    step.num = step.num-1
  } else {
    if (t>1) {
      t = t-1
      period = get.story.period(es=es,t=t)
      step.num = length(period$steps)
    } else {
      t = 1
      step.num = 1
      start = TRUE
    }
  }
  
  if (update.es) {
    es$t = t
    es$step.num = step.num
    es$wait.for.answer = FALSE
  }
  
  return(list(t=t, step.num=step.num, start=start))
}


story.next.step = function(es, t=es$t, step.num=es$step.num, update.es=TRUE) {
  restore.point("story.next.step")
  
  period = get.story.period(es,t)
  num.steps = length(period$steps)
  
  if (step.num < num.steps) {
    step.num = step.num+1
  } else {
    if (t<es$T) {
      t = t+1
      step.num = 1
    } else {
      return(list(t=t, step.num=1, end=TRUE))
    }
  }
  
  if (update.es)
    es$t = t; es$step.num = step.num
  
  return(list(t=t, step.num=step.num, end=FALSE))
}

shiny.tell.step.task = function(app=getApp(),es=app$es, t=es$t, step.num=es$step.num) {
  restore.point("shiny.tell.step.task")
  es$t = t; es$step.num = step.num
  
  period = get.story.period(es,t)
  st = period$steps[[step.num]]
  html = compile.story.txt(c(st$tell,st$ask), em=es$em, t=t,out = "html")
  html = paste0("<h4>", t,".", step.num,"</h4>", html)
  setUI(id = "tellUI",HTML(html))
  setUI(id = "answerUI",HTML(""))
  
  lines = get.story.step.lines(es = es,t = t,step = step.num,solved=FALSE,previous.steps = TRUE)
  shiny.plot.lines(em=es$em,lines)
}

shiny.tell.step.sol = function(app=getApp(),es=app$es, t=es$t, step.num=es$step.num) {
  restore.point("shiny.tell.step.sol")
  
  es$t = t; es$step.num = step.num
  
  period = get.story.period(es,t)

  st = period$steps[[step.num]]
  html = compile.story.txt(c(st$success), em=es$em, t=t,out = "html")

  setUI(id = "answerUI",HTML(html))

  lines = get.story.step.lines(es = es,t = t,step = step.num,solved=TRUE,previous.steps = TRUE)
  shiny.plot.lines(em=es$em,lines=lines)
  
  dygraph =story.step.dyplot(es=es,t=t,step = step.num,solved = TRUE,vars = es$timelineVars)
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


html.table = function(content, nrow=NULL,ncol=NULL) {
  #content = 1:10; nrow=NULL; ncol=2
  restore.point("html.table")
  content = sapply(content, as.character)
  n = length(content)
  if (is.null(nrow) & is.null(ncol)) {
    nrow=n
    ncol=1
  } else if (is.null(nrow)) {
    nrow = ceiling(n/ncol)
  } else if (is.null(ncol)) {
    ncol = ceiling(n/nrow)
  }
  
  txt = sapply(1:nrow, function(r) {
    inds = seq.int(1+(r-1)*ncol,min(r*ncol,n))
    rtxt = paste0("  <td>",content[inds],"</td>",collapse="\n")
    rtxt = paste0("<tr>\n",rtxt,"\n<tr>")
    rtxt
  })
  txt = paste0("<table>",paste0(txt,collapse="\n"),"</table>")
  txt
}