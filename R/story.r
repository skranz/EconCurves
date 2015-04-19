
examples.story = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  es = load.story("ThreeEq_G_langfristig")
  init.story(es)
  
  em = es$em
  sim = em$sim
  par(mfrow=c(1,2),oma=c(0,0,0,0))
  tell.step.task.on.console(es,t=2,step=1)
  tell.story.on.console(es = es,t.start = 1,step.start = 1)
  
  dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)
}


load.story = function(storyId, file=paste0(storyId,".yaml"), dir=get.ec()$stories.path, ec = get.ec()) {
  restore.point("load.story")
  
  tt = load.struct(name="story",file = paste0(dir,"/",file),typeName = "story")
  es = as.environment(tt.object(tt,1))
  es
}

init.story = function(es) {
  restore.point("init.story")
  init.story.periods(es)
  em = load.model(es$modelId)
  init.model(em)
  init.model.scen(em,scen.name = es$scenarioId)
  init.model.shocks(em,shocks = es$shocks)
  simulate.model(em,T=es$T)
  es$em = em
}

step.task.symbols = function(step) {
  restore.point("step.task.symbols")
  symbols = unique(unlist(lapply(step$task, function(ta) ta$symbol)))  
}

init.story.periods = function(es) {
  es$periods = lapply(es$periods, function(period) {
    symbols = NULL
    for (i in seq_along(period$steps)) {
      step = period$steps[[i]]
      symbols = unique(c(symbols, step$show, paste0("lag_",step$lagshow)))
      step$start.symbols = symbols
      symbols = unique(c(symbols, step.task.symbols(step)))
      step$end.symbols = symbols
      
      #restore.point("jdvndjnvduvhz")
      #str = step$tell
      try(Encoding(step$tell) <- "UTF-8", silent=TRUE)
      try(Encoding(step$ask) <- "UTF-8", silent=TRUE)
      try(Encoding(step$success) <- "UTF-8", silent=TRUE)
      
      period$steps[[i]] = step
    }
    period$symbols = symbols
    period
  })
}


get.story.period = function(es,t) {
  es$periods[[t]]
}

get.story.step.lines = function(es, t, step, solved=FALSE, previous.steps=TRUE) {
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
  compute.symbol.lines(t = t,em = em,symbols = symbols)
}

compile.story.txt = function(txt, out="text",val =as.list(em$sim[t,,drop=FALSE]),  em=NULL,t=1) {
  restore.point("compile.story.txt")
  
  txt = whisker.render(txt, data=val)
  if (out=="text") {
    txt = gsub("$","",txt, fixed=TRUE)
  }
  txt
  
}

tell.story.on.console = function(es, t.start=1, step.start=1) {
  restore.point("tell.story.on.console")
  
  for (t in 1:es$T) {
    period = get.story.period(es=es,t=t)
    for (s in seq_along(period$steps)) {
      step = period$steps[[s]]
      cat(paste0("\n",t,".", s,":"))
      tell.step.task.on.console(es,t,s)
      readline(prompt="[Press Enter to continue] [Esc to stop]")
      if (length(step$task)>0) {
        tell.step.sol.on.console(es,t,s)
        readline(prompt="[Press Enter to continue] [Esc to stop]")
      }
    }
  }
  
}

tell.step.task.on.console = function(es, t=1, step=1) {
  restore.point("tell.step.task.on.console")
  period = get.story.period(es,t)
  st = period$steps[[step]]
  tell = compile.story.txt(st$tell, em=es$em, t=t)
  ask = compile.story.txt(st$ask, em=es$em, t=t)
  
  
  cat(paste0("\n",tell))
  cat(paste0("\n",ask,"\n"))
  
  lines = get.story.step.lines(es = es,t = t,step = step,solved=FALSE,previous.steps = TRUE)
  plot.lines(em=es$em,lines)
}

tell.step.sol.on.console = function(es, t=1, step=1) {
  restore.point("tell.step.sol.on.console")
  
  st = es$periods[[t]]$steps[[step]]

  success = compile.story.txt(st$success, em=es$em, t=t)

  cat(paste0("\n",success))
  lines = get.story.step.lines(es = es,t = t,step = step,solved=TRUE,previous.steps = TRUE)
  plot.lines(em = em,lines=lines)

}


compute.symbol.lines = function(t, em, symbols) {
  restore.point("compute.symbol.lines")
  lag = symbols[str.starts.with(symbols,"lag_")]
  cur = setdiff(symbols,lag)
  
  cur.lines = lapply(em$panes, function(pane) {
    compute.pane.lines(pane = pane,em = em,t=t,symbols=cur)
  })
  cur.lines = do.call(c,cur.lines)
  names(cur.lines) = sapply(cur.lines, function(line) line$base)

  
  lag.base = str.right.of(lag,"lag_")
  lag.t = ifelse(t>1,t-1,t)
  lag.lines = lapply(em$panes, function(pane) {
    compute.pane.lines(pane = pane,em = em,t=lag.t,symbols=lag.base,level = 2)
  })
  lag.lines = do.call(c, lag.lines)
  names(lag.lines) = sc("lag_",sapply(lag.lines, function(line) line$base))
  
  c(cur.lines, lag.lines)# [symbols]
}
