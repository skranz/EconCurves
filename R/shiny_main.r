
examples.shiny.stories = function() {
  library(EconCurves)
  set.restore.point.options(display.restore.point = TRUE)

  add.restore.point.test(scenario.test = function(env,name,...) {
    if (exists("em",env,inherits = FALSE)) {
      if (is.null(env$em) & name!="init.story") {
        stop("em is null")
      }
      #if (!is.null(env$em$T) & length(env$em$T)==0) {
      #  stop("length(em$T) ==0")
      #}
      
    }
  })

  setwd("D:/libraries/EconCurves/EconCurves")
  #setwd("~/libraries/EconCurves")
  init.ec()
  ec = get.ec()
  load.collection("makro.yaml")
  load.collection("hotelling.yaml")

  app = shinyStoriesApp(ec = ec)
  
  
  #coll.run.story.btn(storyId = "GreenParadoxQuiz")
  
  runEventsApp(app,launch.browser = rstudio::viewer)
  runEventsApp(app,launch.browser = TRUE)

}

load.collection = function(file, dir = paste0(ec$main.path,"/collections"), stories.dir = ec$stories.path, models.dir = ec$models.path, ec=get.ec()) {
  restore.point("load.collection")
  ec$collection = coll = read.yaml(paste0(dir,"/",file))
  
  story.names = names(coll$stories)
  stories = lapply(story.names, function(id) {
    load.story(storyId = id,dir = stories.dir)
  })
  names(stories) = story.names

  model.names = unique(sapply(stories, function(es) es$modelId))
  models = lapply(model.names, function(id) {
    load.model(id,dir = models.dir)
  })
  names(models) = model.names

  ec$models = models
  ec$stories = stories
 
     
  invisible(ec)
}

shinyStoriesApp = function(ec = get.ec(),title = "Stories from simple economic models", can.change.code = FALSE, ...) {
  restore.point("shinyStoriesApp")
  
  library(shinyEvents)  
  library(shinyAce)
  library(shinyBS)
  
  app = eventsApp()
  app$ec = ec
  app$can.change.code = can.change.code 
  
  ui = fluidPage(title = title,uiOutput("storiesBaseUI"))

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    # copy ec for a new app instance
    nec = as.environment(as.list(ec))
    #nec$stories = lapply(nec$stories, function(es) {
    #  as.environment(as.list(es))
    #})
    #nec$models = lapply(nec$models, function(em) {
    #  as.environment(as.list(em))
    #})
    app$ec = nec
    
  }, app=app)
  
  app$ui = ui
  
  ec$choose.ui = stories.choose.ui()
  setUI("storiesBaseUI",ec$choose.ui)
  app
}


stories.choose.ui = function(app=getApp(), ec=app$ec,...) {
  restore.point("stories.choose.ui")
  coll = ec$collection

  cp = lapply(ec$stories, function(es) {
    title = es$title
    Encoding(title) = "UTF-8"
    if (is.null(title)) title = es$storyId
    descr = compile.to.html(es$descr)

    runBtnId = paste0(es$storyId,"_CollRunBtn")
    showStoryBtnId = paste0(es$storyId,"_CollShowStoryBtn")
    showModelBtnId = paste0(es$modelId,"_CollShowModelBtn")
    
    ui = bsCollapsePanel(title = title, value = paste0(es$storyId,"_CollectionPanel"),
      actionButton(runBtnId, label = "Run"),
      actionButton(showModelBtnId, label = "Show Model File"),
      actionButton(showStoryBtnId, label = "Show Story File"),
      HTML(descr)
    )
    
    buttonHandler(runBtnId,if.handler.exists = "skip",fun = coll.run.story.btn, storyId = es$storyId)
    
    buttonHandler(showStoryBtnId,if.handler.exists = "skip",fun = coll.show.story.btn, storyId = es$storyId)
    
    buttonHandler(showModelBtnId,if.handler.exists = "skip",fun = coll.show.model.btn, modelId = es$modelId)

    ui
  })
  names(cp) = NULL
  
  foots = lapply(coll$footnotes, function(ft) {
    title = ft$title
    Encoding(title) = "UTF-8"
    if (is.null(title)) title = ""
    descr = compile.to.html(ft$descr)
    ui = bsCollapsePanel(title = title,
      HTML(descr)
    )
    ui
  })
  names(foots) = NULL
  
  panel.ui = do.call("bsCollapse",c(list(id="collStoriesCollapse"), cp, foots))
  
  title.html = coll$title 
  if (!is.null(title.html)) {
    Encoding(title.html) = "UTF-8"
    title.html = h3(title.html)
  }
  descr.html = compile.to.html(coll$descr)
  ui = list(title.html,HTML(descr.html),panel.ui)
  fluidRow(column(offset=1, width=10,ui))
}

coll.run.story.btn = function(app=getApp(), ec=app$ec, storyId,...) {
  restore.point("coll.run.story.btn")
  app=getApp()
  app$es = as.environment(as.list(ec$stories[[storyId]]))
  modelId = app$es$modelId

  app$em = as.environment(as.list(ec$models[[modelId]]))  
  app$es$em = app$em

  init.story(es = app$es, em=app$em)
  ui = story.ui(es=app$es)

  setUI("storiesBaseUI",ui)
  run.story(app$es)

}


coll.show.story.btn = function(app=getApp(), ec=app$ec, storyId,...) {
  restore.point("coll.show.story.btn")
  es = ec$stories[[storyId]]
  
  setBtn = NULL
  
  if (app$can.change.code) {
    setBtn = actionButton("showStorySetBtn","Apply changes")
  }
  
  ui = list(
    aceEditor("storyYamlAce",value = es$yaml, mode="yaml"),
    bsAlert("showStoryAlert"),
    setBtn,
    actionButton("showStoryExitBtn","Exit")
  )
  buttonHandler("showStoryExitBtn", exit.to.main)
  
  if (app$can.change.code) {
    buttonHandler("showStorySetBtn", storyId = storyId,
      function(app=getApp(), ec=app$ec, storyId,...) {
        new.es = try({
          new.es = load.story(storyId = storyId)
          new.es = init.story(es = new.es)
          new.es
        })
        if (is(new.es,"try-error")) {
          createAlert(session = app$session,"showStoryAlert",title = "Error:",content = as.character(new.es),style = "warning")
        } else {
          ec$stories[[storyId]] = new.es
          createAlert(session = app$session,"showStoryAlert",title = "Changes are applied.",style = "success")
          
        }
      }
    )
  }
  setUI("storiesBaseUI",ui)
}



coll.show.model.btn = function(app=getApp(), ec=app$ec, modelId,...) {
  restore.point("coll.show.model.btn")
  em = ec$models[[modelId]]
  ui = list(
    aceEditor("modelYamlAce",value = em$yaml, mode="yaml"),
    actionButton("showStoryExitBtn","Exit")
  )
  buttonHandler("showStoryExitBtn", exit.to.main)
  setUI("storiesBaseUI",ui)
}


exit.to.main = function(app=getApp(),ec = app$ec,...) {
  if (is.null(ec$collection)) return()
  
  setUI("storiesBaseUI",ec$choose.ui)
 
  
}

compile.to.html = function(txt) {
  if (is.list(txt)) {
    return(lapply(txt,compile.to.html))
  }
  
  if (is.null(txt)) return("")
  
  restore.point("compile.to.html")
  Encoding(txt) = "UTF-8"
  html =  markdownToHTML(text=txt, fragment.only=TRUE, encoding="UTF-8")
  html
}