green.paradox = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  set.restore.point.options(display.restore.point = TRUE)
  
  init.ec()
  ec = get.ec()
  em = load.model("GreenParadox")
  sim = simulate.model(em)
  sim$scenario = "baseline"
    
  es = load.story("GreenParadox_quiz")
  
  #options(warn=3)
  sim = init.story(es)
  app = shinyScenryApp(es = es)
  
  runEventsApp(app,launch.browser = rstudio::viewer)
  
  sim = init.scenry.part(es,2)
  rbind(sim[1,],sim[21,])
  library(ggplot2)
  
  ggplot(data=sim, aes(x=t,y=R,color=.scen,fill=.scen)) + geom_line(size=1.5)
  ggplot(data=sim, aes(x=t,y=cumR,color=.scen,fill=.scen)) + geom_line(size=1.2)
  ggplot(data=sim, aes(x=t,y=p,color=.scen,fill=.scen)) + geom_line(size=1.2)
  ggplot(data=sim, aes(x=t,y=m,color=.scen,fill=.scen)) + geom_line(size=1.2)
  ggplot(data=sim, aes(x=t,y=m_growth,color=.scen,fill=.scen)) + geom_line(size=1.2)

  

}



init.scenry = function(es,em=es$em,...) {
  init.scenry.part(es,1)
  if (length(es$rcode)>0) {
    ec = get.ec()
    for (file in es$rcode)
      source(paste0(ec$stories.path,"/",file))
  }
  
}

scenry.check.question = function(answered, correct, qu,...) {
  restore.point("scenry.check.question")
  #cat("\nGiven answer was ", correct)
  if (!answered) {
    txt = "You have not yet answered the question."
    html = HTML(txt)
  } else {
    
    txt = as.character(qu$expl)
    if (correct) {
      txt = paste0("<b>Correct!</b> ", txt)
    } else {
      txt = paste0("<b>Not correct.</b> ",txt)
    }
    txt = compile.part.txt(txt)
    html = wellPanel(HTML(txt))
  }
  setUI(qu$explId,html)
}

init.scenry.part.questions = function(part) {
  restore.point("init.scenry.part.questions")
  
  # init statements
  qu.ind.base = 0
  part$stas = lapply(seq_along(part$statements), function(ind) {
    st = init.statement(part$statements[[ind]],qu.ind = ind+qu.ind.base)
    st$ui = statement.ui(st = st, check.fun=scenry.check.question)
    st
  })
  
  
  
  ui.li = lapply(part$stas, function(st) st$ui)
  
  qu.ind.base = qu.ind.base + length(part$stas)+1
  if (!is.null(part[["quiz"]])) {
    part$quizes = lapply(seq_along(part$quiz), function(i) {
      qu = part$quiz[[i]]
      qu = init.quiz(qu, quiz.id = paste0("scenry_quiz__",i))
      qu$ui = quiz.ui(qu,in.well.panel = FALSE)
      add.quiz.handlers(qu=qu,check.fun = scenry.check.question,set.ui=FALSE)
      qu
    })
    qu.ui.li = lapply(part$quizes, function(qu) qu$ui)  
    ui.li = c(ui.li, qu.ui.li)
  }

  names(ui.li) = NULL
  part$qu.ui = ui.li
  if (length(part$qu.ui)==0) part$qu.ui = NULL
  part
}

init.scenry.part = function(es, part.num=es$part.num) {
  restore.point("init.scenry.part")
  es$part.num = part.num
  
  baseline = es$scenario
  
  part = es$parts[[part.num]]

  part = init.scenry.part.questions(part)
    
  if (!is.null(part[["scens"]])) {
    scens = lapply(part$scens, function(scen) {
      customize.baseline(baseline, scen=scen)
    })
  } else if (!is.null(part[["params"]])) {
    params = scenry.parse.params(part$params)
    
    grid = expand.grid(params)
    scens = lapply(1:NROW(grid), function(i) {
      customize.baseline(baseline, params=grid[i,])
    })
    names(scens) = paste0("scen", seq_along(scens))
  } else {
    scens = list(baseline=baseline)
  }
  es$scens = scens
  
  # Simulate all scenarios
  es$sim.li = simulate.scenarios(es$em, scens, return.list=TRUE)
  es$sim = em$sim = bind_rows(es$sim.li)  

  es$part = part
  
  invisible(es$sim)
}


#' Simulate multiple scenarios
#' 
#' @param em the model
#' @param scens a list of scenarios
#' @param scen.params A list with different parameters used in the different scenarios
#' @param base.scen The basic scenario which specifies all the parameters that are not changed by scen.params
#' @param lapply.fun the lapply function, may change e.g. to mcapply to simulate scenarioes parallely on a multicore.
simulate.scenarios = function(em, scens=NULL, scen.params=NULL, baseline=em$scenario, return.list=FALSE,lapply.fun = lapply) {
  restore.point("simulate.scenarios")
  
  if (is.null(scens) & !is.null(scen.params)) {
    grid = expand.grid(scen.params)
    scens = lapply(1:NROW(grid), function(i) {
      customize.baseline(baseline, params=grid[i,])
    })
    names(scens) = paste0("scen", seq_along(scen))
  }
  
  sim.li = lapply.fun(seq_along(scens), function(i) {
    restore.point("simulate.scenarios.inner")
    scen.name = names(scens)[i]
    scen = scens[[i]]
    sim = simulate.model(em=em, scen=scen)
    sim = cbind(data.frame(scenario = scen.name), sim)
    sim
  })
  if (return.list) return(sim.li)
  bind_rows(sim.li)  
}

customize.baseline = function(baseline, scen=NULL, params=list()) {
  restore.point("customize.baseline")
  
  
  new.scen = baseline
  
  if (length(params)==0) {
    params[names(scen$params)] = scen$params
  }
  new.scen$params = overwrite.defaults(new.scen$params, params)
  new.scen$axis = overwrite.defaults(new.scen$axis, scen$axis)
  
  new.scen
}

overwrite.defaults = function(defaults, new) {
  defaults[names(new)] = new
  defaults
}




shinyScenryApp = function(es,...) {
  restore.point("shinyStoryApp")
  
  library(shinyEvents)  
  library(shinyAce)
  library(shinyBS)
  
  app = eventsApp()
  app$es = es
  ui = scenry.ui()
  ui = fluidPage(title = es$storyId,ui)

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    app$es = as.environment(as.list(es))
  }, app=app)
  
  app$ui = ui
  scenry.show.part(app = app,part.num = 1, init.part=TRUE)
  app
}


scenry.ui = function(app=getApp(), scela) {
  restore.point("scela.ui")
  ui = list(
    column(5,
      actionButton("scenryPrevBtn","<"),
      actionButton("scenryNextBtn",">"),
      actionButton("scenryForwardBtn",">>"),
      uiOutput("scenryTellUI")
    ),
    column(7,
      uiOutput("scenryOutputUI")  
    )
  )
  buttonHandler("scenryNextBtn", scenry.next.btn.click)
  buttonHandler("scenryForwardBtn", scenry.forward.btn.click)
  buttonHandler("scenryPrevBtn", scenry.prev.btn.click)
  buttonHandler("scenryExitBtn", exit.to.main)

  ui
}


scenry.run.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("scenry.run.btn.click")
  res = try(scenry.set.params())
  scenry.show.part(es=es,part.num=es$part.num, init.part=TRUE)
}


scenry.next.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("scenry.next.btn.click")

  if (es$part.num == length(es$parts)) return()

  es$part.num = es$part.num +1
  scenry.show.part(es=es,part.num=es$part.num, init.part=TRUE)
}


scenry.prev.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("scenry.prev.btn.click")

  if (es$part.num == 1) return()

  es$part.num = es$part.num -1
  scenry.show.part(es=es,part.num=es$part.num, init.part=TRUE)
}


scenry.show.part = function(app=getApp(),es=app$es, part.num = es$part.num, init.part=!identical(es$part.num,part.num)) {
  restore.point("scenry.show.part")
  
  es$part.num = part.num
  if (init.part) {
    init.scenry.part(es = es)
    part = es$part
  } else {
    part = es$part
  }
  # copy simulation into globalenv to facilitate development of plots
  assign("sim",es$sim,envir = globalenv())
  
  
  if (is.null(part$title)) part$title = ""
  
  html = compile.part.txt(c(part$tell), es=es,out = "html")
  html = paste0("<h4>", part.num," ", part$title, "</h4>", html)
  
  params.ui = scenry.part.params.ui(es=es,part.num = part.num)
  
  tell.ui = list(
    HTML(html),
    params.ui,
    actionButton("scenryRunBtn","Run")
  )

  if (length(part$background)>0) {
    bg.ui = compile.part.txt(c(part$background), es=es,out = "html")
    bg.ui = paste0("<h4>", part.num," ", part$title, "</h4>", bg.ui)
    bg.ui = HTML(bg.ui)
  } else {
    bg.ui = NULL
  }
  
  tabs = list(
    tabPanel(title = "Sim", tell.ui)
  )
  if (!is.null(part$qu.ui)) {
    tabs = c(tabs,list(tabPanel(title = "Quiz", part$qu.ui)))
  }
  if (!is.null(bg.ui)) {
    tabs = c(tabs,list(tabPanel(title = "Background", bg.ui)))
  }
  tabs = c(tabs, list(
    tabPanel(title = "Baseline"),
    tabPanel(title = "Model",
      aceEditor("scenryModelYamlAce",value = es$em$yaml, mode="yaml")
    )    
  ))

  tabset = do.call(tabsetPanel, tabs)
  
  setUI(id = "scenryTellUI", tabset)

  buttonHandler("scenryRunBtn", scenry.run.btn.click)
  
  # Show plots 
  output.ui = scenry.part.output.ui(es=es, part.num = part.num)
  setUI(id = "scenryOutputUI",output.ui)
  for (i in seq_along(es$plots)) {
    plotId = names(es$plots)[i]
    setPlot(id = plotId, es$plots[[i]], quoted=TRUE)
  }
  
  
}

scenry.part.output.ui = function(app= getApp(),es=app$es, part.num = es$part.num) {
  restore.point("scenry.output.ui")
  part = es$parts[[part.num]]
  
  plots = part$plots
  if (is.null(plots)) {
    plots = es$defaults$plots
  }
  ref = names(plots)
  
  rc = ref.to.rowcol(ref)
  
  col.share = round((rc$colspan / max(rc$end.col)*100))
  
  part$plotIds = sc("scenryPlot_",seq_along(plots),"__",col.share)
  names(plots) = part$plotIds
  
#  names(plots) = sc("scenryPlot_",seq_along(plots))
 
  
  if (length(plots)==0) {
    es$plots = NULL
    return(NULL)
  }
  
  plots = lapply(plots, function(plot.txt) {
    if (!has.substr(plot.txt,"(")) { #)
      new.txt = paste0(plot.txt,"(es$sim)")
      plot = parse.as.call(new.txt)
    } else {
      plot = parse.as.call(plot.txt)
      plot = substitute.call(plot,list(. = quote(es$sim)))
    }
  })
  es$plots = plots
  

  
  li = lapply(seq_along(plots), function(i) {
    plotId = part$plotIds[[i]]
    clickId = paste0("scenryPlot_",i,"__click")
    #changeHandler(id=clickId, shiny.pane.click, pane.name=pane$name)
    plotOutput(outputId = plotId,click = clickId, width="100%",height="250px")
    #plotOutput(outputId = plotId,click = clickId, width="auto",height="auto")
  })
  names(li) = NULL
  ui = HTML(html.table(li,ref=ref,ncol=2))

  return(ui)
}

scenry.part.params.ui = function(app= getApp(),es=app$es, part.num = es$part.num) {
  restore.point("scenry.params.ui")
  
  
  part = es$parts[[part.num]]
  if (is.null(part$params) & is.null(part$scens)) return(NULL)
  if (!is.null(part$scens)) {
    li = lapply(part$scens, function(scen) scen$params)
    yaml = as.yaml(li)
  } else {
    yaml = as.yaml(part$params)
  }
  txt = sep.lines(yaml)
  fontSize = 12
  height = max((fontSize * 1.5) * length(txt),30)    
  aceEditor("scenryParamsEdit",value=yaml,mode="yaml", showLineNumbers = FALSE,height=height)
}

scenry.set.params = function(app= getApp(),es=app$es) {
  
  
  part.num = es$part.num
  part = es$parts[[part.num]]
  if (is.null(part$params) & is.null(part$scens)) return(NULL)
  yaml = getInputValue("scenryParamsEdit")
  li = read.yaml(text = yaml)

  restore.point("scenry.set.params")
  
  if (!is.null(part$scens)) {
    for (sc in intersect(names(li), names(part$scens))) {
      vals = scenry.parse.params(li[[sc]])
      part$scens[[sc]]$params[names(li[[sc]])] = vals
    }
  } else {
    vals = scenry.parse.params(li)
    part$params[names(li)] = li 
  }
  es$parts[[part.num]] = part
}

scenry.parse.params = function(li) {
  restore.point("scenry.parse.params")
  li = lapply(li, safe_parse_eval_number)  
  li
}

compile.part.txt = function(txt, es=NULL, out="html") {
  if (out=="text") {
    txt = gsub("$","",txt, fixed=TRUE)
  } else if (out=="html") {
    txt = markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)
    #Encoding(txt) <- "UTF-8"
    txt
  }
  txt
}

scenPlot = function(sim,x,y,t=1,size=1.2, color="black",title=NULL,...) {
  myt = t
  d = filter(sim, t==myt)
  library(ggplot2)
  p = ggplot(data=d, aes_string(x=x,y=y)) + geom_line(size=size, color=color)
  if (!is.null(title)) p = p + ggtitle(title)
  p
}
