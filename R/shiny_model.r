examples.shiny.model = function() {
  set.restore.point.options(display.restore.point = TRUE)
  
  # Model builder
  ec = init.ec()
  em = load.model("Ger3Eq")
  #df = testwise.init.model(em)$df
  
  mb = init.mb("Ger3Eq")
  app = eventsApp()
  app$mb = mb
  
  model.ui = model.yaml.ui()
  init.ui = model.init.ui()
  tabPanels = list(
    tabPanel("Model", model.ui, value="modelPanel"),
    tabPanel("Solve init", init.ui, value="initPanel"),
    tabPanel("Solve t", value="tPanel")
  )
  tabsetUI = fluidPage(
    do.call(tabsetPanel,c(tabPanels,id="modelTabset"))
  )
  
  app$ui = shinyUI(tabsetUI)
  
  runEventsApp(app, launch.browser = rstudio::viewer)
  
  ui
  
  navbarUi = navbarPage(
    title="Model Builder",
    position = "fixed-top",
    tabPanels,
    list(tags$style(type="text/css", "body {padding-top: 70px;}"))
  )

}

init.mb = function(modelId, file=paste0(modelId,".yaml"), dir=get.ec()$models.path, ec = get.ec()) {
  restore.point("init.mb")
  
  
  long.file = paste0(dir,"/",file)
  file.exists = file.exists(long.file)
  
  if (file.exists) {
    yaml = readLines(long.file,warn = FALSE)
    em = try(load.model(modelId))
    if (is(em,"try-error")) {
      em = NULL
    }
  } else {
    yaml = readLines(paste0(dir,"/template.yaml"),warn=FALSE)
    replace.whiskers(yaml, list(modelId=modelId))
    em = NULL
  }
  yaml = paste0(yaml, collapse="\n")

  mb = as.environment(list(
    modelId=modelId,
    long.file = long.file,
    file=file,
    dir=dir,
    file.exists = file.exists,
    em = em,
    yaml = yaml
  ))
  mb
  
}

model.init.ui =  function(app=getApp(), mb=app$mb ,...) {
  restore.point("model.init.ui")
  
  ui = list(
    actionButton("modelInitRunBtn","Init"),
    uiOutput("modelInitConsole"),
    uiOutput("modelInitMain")
  )
  #aceHotkeyHandler("modelInitRunKey", click.model.init.run)
  buttonHandler("modelInitRunBtn", click.model.init.run)
  ui
}  

show.model.msg = function(..., form="init", app=getApp()) {
  txt = paste0(..., collapse="\n")
  
  if (form=="init") {
    html = p(txt)
    setUI("modelInitConsole", html)
  }
}

click.model.init.run = function(app=getApp(), mb=app$mb,...) {
  restore.point("click.model.init.run")
  updateTabsetPanel(app$session, "modelTabset", selected = "initPanel") 

  em = mb$em
  if (is.null(em)) {
    show.model.msg("No working model loaded.", form="init")
    return()
  }
  res = testwise.init.model(em)
  df = res$df
  
  
  
  df.html = HTML(hwrite(df[,c("step","finite.vals","na.vals","num.warn","num.err","changed.vals")]))
  
  has.err = df$err.warn.txt != ""
  all.err = sc("<h5>",df$step[has.err],"</h5>\n ",df$err.warn.txt[has.err], collapse="\n<hr>")
  
  clu.df = res$li[[length(res$li)]]$clu.df
  val.df = clu.df[,c("var","val")]

  main.panel = tabPanel("init.model",
    df.html,hr(),
    h5("Error and Warnings:"),HTML(all.err),
    HTML(hwrite(val.df))
  )
  
  steps.panels = lapply(which(df$has.clu.df), function(row) {
    name = df$step[row]
    clu.df = res$li[[row]]$clu.df
    clu.ui = lapply(unique(clu.df$cluster), cluster.ui, clu.df=clu.df, exo=em$init.exo)
    txt = df$err.warn.txt[row]
    if (txt=="") {
      err.ui = NULL
    } else {
      err.ui = wellPanel(p(txt))
    }
    
    do.call(tabPanel,c(list(title=name,err.ui),clu.ui))
  })


  ui = do.call(tabsetPanel, c(list(main.panel),steps.panels))
  setUI("modelInitMain",ui)
}


model.yaml.ui = function(app=getApp(), mb=app$mb ,...) {
  restore.point("model.yaml.ui")
  
  ui = list(
    aceEditor("modelYamlAce",value = mb$yaml, mode="yaml",
      showLineNumbers = FALSE,debounce = 0,
      hotkeys = list(
        modelYamlSaveKey=list(win="Ctrl-S",mac="CMD-S"),
        changeToInitKey = list(win="Ctrl-I",mac="CMD-I")
      )
    ),
    actionButton("modelSaveBtn","Save and Update")
  )
  aceHotkeyHandler("modelYamlSaveKey", click.model.save.update)
  aceHotkeyHandler("changeToInitKey", click.model.init.run)
  buttonHandler("modelSaveBtn", click.model.save.update)
  ui
}

set.init.tabset = function(app=getApp(),...) {
  restore.point("set.init.tabset")
  
  updateTabsetPanel(app$session, "modelTabset", selected = "initPanel") 
  click.model.init()
}

withErrWarn <- function(expr, quoted=NULL, env = parent.frame()) {
    if (!is.null(quoted)) {
      expr = quoted
    } else {
      expr = substitute(expr)
    }

    myWarnings <- NULL
    wHandler <- function(w) {
        myWarnings <<- c(myWarnings, list(w))
        #invokeRestart("muffleWarning")
    }
    myErrors <- NULL
    eHandler <- function(w) {
        myErrors <<- c(myErrors, list(w))
    }
    
    val <- try(withCallingHandlers(eval(expr,env), warning = wHandler, error=eHandler))
    list(value = val, warnings = myWarnings, errors=myErrors,ok=is.null(myWarnings) & is.null(myErrors))
} 


testwise.init.model = function(em) {
  restore.point("testwise.init.model")
  
  log.li = list()
  
  log = function(expr) {
    env = parent.frame()
    quoted = substitute(expr)
    res = withErrWarn(quoted=quoted, env=env)
    res$step = step
    res$call = quoted
    res$clu.df = get("clu.df",env)
    log.li[[step]] <<- res[-1]
    invisible(res$value)
  }
  
  clu.df <- NULL
  step = "init.model"
  log(init.model(em,skip.cluster.equations = TRUE)) 

  step = "init.scen"
  log(init.model.scen(em, skip.cluster.init = TRUE))
  
  step = "make.init.eqs.and.exo"
  log(make.init.eqs.and.exo(em))

  step = "cluster.equations"
  log(clu.df <- cluster.equations(em$init.eqs, exo=names(em$init.exo), funs=em$var.funs, skip.big=TRUE,solve.symbolic = FALSE,skip.eat = TRUE))

  step = "vals.cluster.equations"
  log(clu.df$val <- eval.cluster.df(clu.df, exo=em$init.exo))

  step = "eat.calls"
  log(clu.df <- eat.from.cluster(clu.df, cluster=1))
  
  step = "vals.eat.calls"
  log(clu.df$val <- eval.cluster.df(clu.df, exo=em$init.exo))
  
  step = "solve.single.symbolic"
  log(clu.df <- solve.symbolic.cluster.df(clu.df,skip.big = TRUE))

  step = "vals.solve.single.symbolic"
  log(clu.df$val <- eval.cluster.df(clu.df, exo=em$init.exo))
  
  step = "solve.system.symbolic"
  log(clu.df <- solve.symbolic.cluster.df(clu.df,skip.big = FALSE))

  step = "vals.solve.system.symbolic"
  log(clu.df$val <- eval.cluster.df(clu.df, exo=em$init.exo))


  # combine logs to a data.frame  
  li = lapply(seq_along(log.li), function(ind) {
    el = log.li[[ind]]
    txt = sc(seq_along(el$warnings), ". warning: ", sapply(el$warnings, as.character),collapse="\n\n")
    txt = c(sc(seq_along(el$errors), ". error: ", sapply(el$errors, as.character),collapse="\n\n"),txt)
    if (length(txt)==0) txt = ""

    list(ind = ind,step=el$step, ok=el$ok, num.warn=length(el$warnings), num.err = length(el$errors), err.warn.txt = txt, has.clu.df=!is.null(el$clu.df))
  })
  li.df = rbindlist(li)
  
  #tdf = log.li[["vals.eat.calls"]]$clu.df
  
  # Adapt for value computation
  li.df$compute.vals = FALSE
  li.df$na.vals = li.df$finite.vals = NA_integer_
  li.df$vals.txt = ""
  li.df$changed.vals = ""
  
  vals.rows = which(str.starts.with(li.df$step,"vals"))
  if (length(vals.rows)>0) {
    for (row in vals.rows) {
      clu.df = log.li[[row]]$clu.df
      li.df$compute.vals[row-1] = FALSE
      li.df$na.vals[row-1] = sum(is.na(clu.df$val))
      li.df$finite.vals[row-1] = sum(is.finite(clu.df$val))
      li.df$vals.txt[row-1] = li.df$err.warn.txt[row]
      log.li[[row-1]]$clu.df = clu.df
    }
    li.df = li.df[-vals.rows,]
    log.li = log.li[-vals.rows]
  }
  
  # log changes in values between steps
  rows = which(li.df$has.clu.df)
  for (i in seq_along(rows)[-1]) {
    pclu.df = log.li[[rows[i-1]]]$clu.df
    clu.df = log.li[[rows[i]]]$clu.df
    
    pvals = pclu.df$val
    names(pvals) = pclu.df$var

    vals = clu.df$val
    names(vals) = clu.df$var
    vals = vals[names(pvals)]
    
    changed = abs(vals-pvals) > 1e-7
    changed[is.na(pvals)] = FALSE
    changed[is.na(changed)] = TRUE
    
    changed.var = paste0(names(changed)[changed],collapse=",")
    li.df$changed.vals[rows[i]] = changed.var
  }
  
  list(df=li.df, li=log.li)
}

click.model.save.update = function(app=getApp(), mb=app$mb,...) {

  new.yaml =paste0(getInputValue("modelYamlAce"), collapse="\n") 
  restore.point("click.model.save.update")
  
  
  # Create a backup
  backup.file = paste0(mb$modelId,".bak")
  writeLines(mb$yaml, paste0(mb$dir,"/", backup.file))
  
  # Save yaml
  mb$yaml = new.yaml
  writeLines(mb$yaml, mb$long.file)
  
  # Try to load.yaml
  em = try(load.model(mb$modelId))
  if (is(em,"try-error")) {
    em = NULL
    cat("\nSaved but error when parsing.")  
  } else {
    cat("\nSaved and successfully updated.")  
  }
  mb$em = em  
  
}

