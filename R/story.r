
examples.story = function() {
  set.restore.point.options(display.restore.point = !TRUE)

  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  #es = load.story("ThreeEq_G_langfristig")
  es = load.story("ThreeEqFixedM_G_langfristig")
  es = load.story("IS_LM_PC_G_kurzfristig")
  init.story(es)
  par(mfrow=c(1,2),oma=c(0,0,0,0))
  tell.story.on.console(ask=TRUE,es = es,t.start = 1,step.start = 1, mfrow=c(1,2))
  
  tell.story.on.console(ask=TRUE,es = es,t.start = 2,step.start = 8, mfrow=c(1,2))

  em = es$em
  sim = em$sim
  dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)


  tell.step.task.on.console(es,t=2,step=1)
  
}


load.story = function(storyId, file=paste0(storyId,".yaml"), dir=get.ec()$stories.path, ec = get.ec()) {
  restore.point("load.story")
  
  tt = load.struct(name="story",file = paste0(dir,"/",file),typeName = "story")
  es = as.environment(tt.object(tt,1))
  es
}

init.story = function(es) {
  restore.point("init.story")
  es$t = es$step.num = 1
  es$wait.for.answer = FALSE
  es$tol = 0.07
    
  init.story.periods(es)
  em = load.model(es$modelId)
  init.model(em)
  if (is.null(es$scenario))
    es$scenario = em$scenarios[[es$scenarioId]]
  
  init.model.scen(em,scen = es$scenario)
  init.model.shocks(em,shocks = es$shocks)
  simulate.model(em,T=es$T)
  es$em = em
}

step.task.symbols = function(step) {
  restore.point("step.task.symbols")
  tasks = setdiff(names(step$task),"pane")
  symbols = unique(unlist(lapply(step$task[tasks], function(ta) ta$symbol)))
  symbols
}

init.story.periods = function(es) {
  restore.point("init.story.periods")
  #period = es$periods[[2]]
  es$periods = lapply(seq_along(es$periods), function(period.num) {
    period = es$periods[[period.num]]
    symbols = NULL
    i = 1
    for (i in seq_along(period$steps)) {
      step = period$steps[[i]]
      symbols = unique(c(symbols, step$show, sc("lag_",step$lagshow)))
      
      symbols = setdiff(symbols, c(step$hide,sc("lag_",step$laghide) ))
      step$start.symbols = symbols
      symbols = unique(c(symbols, step.task.symbols(step)))
      step$end.symbols = symbols
      
      #restore.point("jdvndjnvduvhz")
      #str = step$tell
      try(Encoding(step$tell) <- "UTF-8", silent=TRUE)
      try(Encoding(step$ask) <- "UTF-8", silent=TRUE)
      try(Encoding(step$success) <- "UTF-8", silent=TRUE)
      
      if (!is.null(step$task))
        step$task$type = get.story.step.task.type(step)
      
      period$steps[[i]] = step
    }
    period$symbols = symbols
    period$period.num = period.num
    period
  })
  
  tperiod.df = data.frame(period=seq_along(es$periods), t.start=seq_along(es$periods), t.end = Inf)
  t = 1
  for (per in seq_along(es$periods)[-1]) {
    period = es$periods[[per]]
    if (is.null(period$t)) {
      t = t+1
    } else {
      t = period$t
    }
    tperiod.df$t.end[per-1] = t-1
    tperiod.df$t.start[per] = t
  }
  es$tperiod.df = tperiod.df
  
  es
}

get.story.step.task.type = function(step) {
  restore.point("get.story.step.task.type")
  types = c("find","shift","select","findPoint")
  not.null = which(sapply(types, function(type) {
    !is.null(step$task[[type]])
  }))
  if (length(not.null)==0) return("unknown")
  
  types[not.null]
}

get.story.period = function(es,t=es$t) {
  if (is.null(es$tperiod.df))
    return(es$periods[[t]])
  
  per = which(es$tperiod.df$t.start<=t & es$tperiod.df$t.end>=t)
  es$periods[[per[1]]]
}

get.period.t = function(es, period.num=period$period.num,period ) {
  if (period.num > length(es$periods))
    return(es$T)
  
  es$tperiod.df$t.start[period.num] 
}

get.story.step.symbols = function(es,t, step.num, solved=TRUE, previous.steps=TRUE) {
  st = get.story.period(es,t)$steps[[step.num]]
  
  if (!solved & previous.steps) {
    symbols = st$start.symbols
  } else if (solved & previous.steps) {
    symbols = st$end.symbols
  } else if (!solved & !previous.steps) {
    symbols = st$show
  } else if (solved & !previous.steps) {
    symbols = c(st$show, names(st$task))
  }
  symbols
}

story.step.dyplot = function(es, t, step, solved=FALSE, previous.steps=TRUE, pane.names = names(es$em$panes), vars = names(es$em$vars)) {
  restore.point("story.step.dyplot")
  
  em = es$em
  sim = em$sim
  sim = sim[,c("t",vars)]
  
  #if (t==1) return()
  
  if (t<NROW(sim)) {
    sim[(t+1):NROW(sim),vars]=NA 
  }
  symbols = get.story.step.symbols(es=es,t=t,step.num=step, solved=solved, previous.steps=previous.steps)

  t.vars = intersect(symbols, vars)
  hidden.vars = setdiff(vars, t.vars)
  if (length(hidden.vars)>0) {
    sim[t,hidden.vars] = NA
  }
  sim
  #cat("\nRefresh timeline: t=",t, " vars = ", paste0(t.vars, collapse=","))
  dyplot.timelines(sim,cols = vars,em = em)

}

get.story.step.lines = function(es, t, step, solved=FALSE, previous.steps=TRUE, pane.names = names(es$em$panes)) {
  restore.point("get.story.step.lines")
  st = get.story.period(es,t)$steps[[step]]
  
  if (!solved & previous.steps) {
    symbols = st$start.symbols
  } else if (solved & previous.steps) {
    symbols = st$end.symbols
  } else if (!solved & !previous.steps) {
    symbols = st$show
  } else if (solved & !previous.steps) {
    symbols = c(st$show, names(st$task))
  }
  compute.symbol.lines(t = t,em = es$em,symbols = symbols, pane.names=pane.names)
}

compile.story.txt = function(txt, out="text",val =as.list(em$sim[t,,drop=FALSE]),  em=NULL,t=1, digits=4) {
  restore.point("compile.story.txt")
  
  if (length(txt)==0) return("")
  
  val = lapply(val, function(v) {
    if (is.numeric(v)) return(signif(v,digits))
    return(v)
  }) 
  
  txt = whisker.render(txt, data=val)
  
  if (out=="text") {
    txt = gsub("$","",txt, fixed=TRUE)
  } else if (out=="html") {
    restore.point("compile.story.txt.2")
    txt = markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)
    #Encoding(txt) <- "UTF-8"
    txt
  }
  txt
  
}


has.click.found = function(click.val, ref.val, axis="xy", tol=0.05, tol.units=c("perc","inches")[1],pane=NULL) {
  restore.point("has.click.found")
  
  if (tol.units=="inches") {
    if (axis=="x") {
      ref.inch = grconvertX(ref.val, from = "user", to = "inches")
      click.inch = grconvertX(click.val[[1]], from = "user", to = "inches")
      dist = abs(ref.inch-click.inch)
    } else if (axis=="y") {
      ref.inch = grconvertY(ref.val, from = "user", to = "inches")
      click.inch = grconvertY(click.val[[length(click.val)]], from = "user", to = "inches")
      dist = abs(ref.inch-click.inch)
    } else if (axis=="xy") {
      riX = grconvertX(ref.val[[1]], from = "user", to = "inches")
      ciX = grconvertX(click.val[[1]], from = "user", to = "inches")
      riY = grconvertY(ref.val[[2]], from = "user", to = "inches")
      ciY = grconvertY(click.val[[2]], from = "user", to = "inches")
      dist = sqrt((riX-ciX)^2+(riY-ciY)^2)
    }
    return(dist<=tol)
  }
  if (tol.units=="perc") {
    xlen = diff(pane$xrange)
    ylen = diff(pane$yrange)
    if (axis=="x") {
      dist = abs(ref.val-click.val[[1]]) / xlen
    } else if (axis=="y") {
      dist = abs(ref.val-click.val[[length(click.val)]]) / ylen
    } else if (axis=="xy") {
      dist = sqrt(((ref.val[[1]]-click.val[[1]])/xlen)^2+
                  ((ref.val[[2]]-click.val[[2]])/ylen)^2)
    }
    return(dist<=tol)
  }
  stop("unknown.tol.units")

} 

get.task.pane = function(es, task) {
  if (!is.null(task$pane)) return(task$pane)
  return(NULL)
}

compute.symbol.lines = function(t, em, symbols, pane.names=names(em$panes)) {
  restore.point("compute.symbol.lines")
  lag = symbols[str.starts.with(symbols,"lag_")]
  cur = setdiff(symbols,lag)
  
  cur.lines = lapply(em$panes[pane.names], function(pane) {
    compute.pane.lines(pane = pane,em = em,t=t,symbols=cur)
  })
  cur.lines = do.call(c,cur.lines)
  names(cur.lines) = sapply(cur.lines, function(line) line$base)

  
  lag.base = str.right.of(lag,"lag_")
  lag.t = ifelse(t>1,t-1,t)
  lag.lines = lapply(em$panes[pane.names], function(pane) {
    compute.pane.lines(pane = pane,em = em,t=lag.t,symbols=lag.base,level = 2)
  })
  lag.lines = do.call(c, lag.lines)
  names(lag.lines) = sc("lag_",sapply(lag.lines, function(line) line$base))
  
  c(cur.lines, lag.lines)# [symbols]
}

check.click.answer = function(es,xy,pane.name,t=es$t, step.num=es$step.num,task=NULL,...) {
  restore.point("check.click.answer")
  em = es$em
  if (is.null(task)) {
    period = get.story.period(es,t)
    step = period$steps[[step.num]]
    task = step$task
  }  
  if (task$type == "shift") {
    ret = check.shift.answer(es=es,xy=xy,em=em,t=t,task=task, pane.name=pane.name)
  } else if (task$type == "find") {
    ret = check.find.answer(es=es,xy=xy,em=em,t=t,task=task, pane.name=pane.name)
  } else if (task$type == "findPoint") {
    ret = check.findPoint.answer(es=es,xy=xy,em=em,t=t,task=task, pane.name=pane.name)
  }

  return(ret)
}

check.shift.answer = function(es,xy,pane.name,task, em=es$em,t=es$t)  {
  restore.point("check.shift.answer")
  #cat("\nAnswer by clicking correctly in the figure.")
  symbols = task$shift$symbol
  
  # wrong pane clicked
  pane = es$em$panes[[pane.name]]
  if (!has.pane.all.symbols(pane, symbols))
    return(FALSE)

  ref.line = compute.symbol.lines(t=t, em=em, symbols=symbols[1], pane.names=pane.name)[[1]]
  line = compute.symbol.lines(t=t, em=em, symbols=symbols[2], pane.names=pane.name)[[1]]
  
  ref.shift = line.to.line.shift(line, ref.line)
  
  point.shift = sign(point.to.line.pos(xy,ref.line))
  if (all(point.shift==ref.shift))
    return(TRUE)
  return(FALSE)
}

# Find a marker
check.find.answer = function(es,xy,pane.name,task, em=es$em,t=es$t, val = as.list(em$sim[t,]), tol=es$tol) {
  restore.point("console.ask.find")
  symbol = task$find$symbol
  ref.val = val[[symbol]]
  pane = em$panes[[pane.name]]
  # wrong pane clicked
  if (!all(symbol %in% names(pane$markers))) {
    return(FALSE)
  }
  axis = pane$markers[[symbol]]$axis
  found = has.click.found(click.val = xy,ref.val = ref.val,axis = axis, tol=tol,pane=pane)
  return(found)
}


# Find a marker
check.findPoint.answer = function(es,xy,pane.name,task, em=es$em,t=es$t, val = as.list(em$sim[t,]), tol=es$tol) {
  restore.point("check.findPoint.answer")

  symbols = task$findPoint$symbol
  pane = em$panes[[pane.name]]
  # wrong pane clicked
  if (!all(symbols %in% names(pane$markers))) {
    return(FALSE)
  }

  ref.val = unlist(val[symbols])

  axis = "xy"
  found = has.click.found(click.val = xy,ref.val = ref.val,axis = axis, tol=tol,pane=pane)
  return(found)
}



