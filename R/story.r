
examples.story = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  es = load.story("ThreeEq_G_langfristig")
  init.story(es)
  par(mfrow=c(1,2),oma=c(0,0,0,0))
  tell.story.on.console(ask=TRUE,es = es,t.start = 1,step.start = 1, mfrow=c(1,2))
  
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
  tasks = setdiff(names(step$task),"pane")
  symbols = unique(unlist(lapply(step$task[tasks], function(ta) ta$symbol)))  
}

init.story.periods = function(es) {

  #period = es$periods[[2]]
  es$periods = lapply(es$periods, function(period) {
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
    period
  })
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

get.story.period = function(es,t) {
  es$periods[[t]]
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

compile.story.txt = function(txt, out="text",val =as.list(em$sim[t,,drop=FALSE]),  em=NULL,t=1) {
  restore.point("compile.story.txt")
  
  if (length(txt)==0) return("")
  txt = whisker.render(txt, data=val)
  if (out=="text") {
    txt = gsub("$","",txt, fixed=TRUE)
  }
  txt
  
}

tell.story.on.console = function(es, t.start=1, step.start=1, mfrow=c(1,1), ask=FALSE, num.attempts=3) {
  restore.point("tell.story.on.console")
  
  es$mfrow = mfrow
  
  par(mfrow=mfrow)
  
  t = 3
  for (t in 1:es$T) {
    period = get.story.period(es=es,t=t)
    for (s in seq_along(period$steps)) {
      step = period$steps[[s]]
      cat(paste0("\n",t,".", s,":"))
      tell.step.task.on.console(es,t,s)
      if (ask & length(step$task)>0) {
        ret = ask.task.on.console(es, t, s, num.attempts=num.attempts)
        if (!ret)
          cat("Let us just proceed as if you were correct. You can try next time.\n")
      } else {
        readline(prompt="[Press Enter to continue] [Esc to stop]")
      }
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
  plot.lines(em = es$em,lines=lines)

}

ask.task.on.console = function(es, t=1, step=1, num.attempts=1) {
  restore.point("ask.task.on.console")
  period = get.story.period(es,t)
  st = period$steps[[step]]
  em = es$em
  
  task = st$task
  if (length(task)==0) return(TRUE)
  if (is.null(task$type)) return(TRUE)
  if (task$type == "unknown") {
    warning("Unknown task type in period", t,"step",step)
    return(TRUE)
  }
  
  pane.name = get.task.pane(es,task)
  
  if (is.null(pane.name)) {
    warning("Could not identify task pane in period", t,"step",step)
    return(TRUE)
  }
  
  
  #cat("\nAnswer by clicking at a correct position in the pane.")
  
  # draw task pane
  par(mfrow=c(1,1))
  lines = get.story.step.lines(es = es,t = t,step = step,solved=FALSE,previous.steps = TRUE, pane.names=pane.name)
  plot.lines(em=es$em,lines, pane.names=pane.name)
  
  ret = TRUE
  for (attempt in 1:num.attempts) {
    if (task$type == "shift") {
      ret = console.ask.shift(es=es,em=em,t=t,task=task, pane.name=pane.name)
    } else if (task$type == "find") {
      ret = console.ask.find(es=es,em=em,t=t,task=task, pane.name=pane.name)
    }
    if (ret) break
    if (!ret & attempt<num.attempts)
      cat("\nSorry that was wrong. Try again.")
    if (!ret & attempt<num.attempts)
      cat("\nSorry, that was ", attempt , "times wrong.")
  }
  
  par(mfrow=es$mfrow)
  return(ret)
  
}

console.ask.shift = function(es, em,t, task, pane.name) {
  restore.point("console.ask.shift")
  #cat("\nAnswer by clicking correctly in the figure.")
  symbols = task$shift$symbol
  ref.line = compute.symbol.lines(t=t, em=em, symbols=symbols[1], pane.names=pane.name)[[1]]
  line = compute.symbol.lines(t=t, em=em, symbols=symbols[2], pane.names=pane.name)[[1]]
  
  #plot.lines(em=es$em,lines=list(ref.line,line), pane.names=pane.name)
  ref.shift = line.to.line.shift(line, ref.line)
  
  xy = unlist(locator(1))
  point.shift = sign(point.to.line.pos(xy,ref.line))
  if (all(point.shift==ref.shift))
    return(TRUE)
  cat("\nUnfortunately, not correct...")
  return(FALSE)
  
}

# Find a marker
console.ask.find = function(es, em,t, task, pane.name, val = as.list(em$sim[t,]), tol=0.15) {
  restore.point("console.ask.find")
  #cat("\nAnswer by clicking correctly in the figure.")
  symbol = task$find$symbol

  #plot.lines(em=es$em,lines=list(ref.line,line), pane.names=pane.name)
  xy = unlist(locator(1))
  ref.val = val[[symbol]]

  pane = em$panes[[pane.name]]
  axis = pane$markers[[symbol]]$axis
  axis.ind = ifelse(axis=="x",1,2)
  
  click.val = xy[axis.ind]
  
  if (axis=="x") {
    ref.inch = grconvertX(ref.val, from = "user", to = "inches")
    click.inch = grconvertX(click.val, from = "user", to = "inches")
  } else if (axis=="y") {
    ref.inch = grconvertY(ref.val, from = "user", to = "inches")
    click.inch = grconvertY(click.val, from = "user", to = "inches")
  }
  dist = abs(ref.inch-click.inch)

  if (dist>tol)
    return(FALSE)  
  
  return(TRUE)
}


line.to.line.shift = function(line, ref.line, num.points=5,...) {
  restore.point("line.to.line.shift")
  n = length(line$x)
  if (n>1) {
    rows = unique(round(seq(1,n,length=num.points)))
  } else {
    rows = 1
  }
  row = 1
  li = lapply(rows, function(row) {
    pos = point.to.line.pos(c(x=line$x[row],y=line$y[row]), ref.line)
    sign(pos)
  })
  df = as.data.frame(do.call(rbind,li))
  if (diff(range(df$x))>=2 | diff(range(df$y))>=2){
    stop("line was not shifted but new line crosses old line")  
  }
  
  x.ind = which.max(abs(df$x))
  y.ind = which.max(abs(df$y))
  return( c(x=df$x[x.ind], y=df$y[y.ind]) )
}

point.to.line.pos = function(xy,line,dim="xy") {
  restore.point("point.to.line.pos")

  line.xy = find.nearest.line.point(xy=xy,line=line,dim=dim)
  #
  #points(xy[1],xy[2])
  #points(line.xy[1],line.xy[2],col="red")
  #lines(x=line$x,y=line$y, col="red")
  xy-line.xy
}

point.to.line.distance = function(xy,line, dim="xy") {
  restore.point("point.to.line.distance")
  
  if (line$type=="marker") {
    if (dim!="xy" & line$axis !=dim) return(Inf)
    dim = line$axis    
  }
  
  if (dim=="xy") {
    dist.vec = (xy[1]-line$x)^2+(xy[2]-line$y^2)
    dist = sqrt(min(dist.vec))
  } else if (dim=="x") {
    dist.vec = abs(xy[1]-line$x)
    dist = min(dist.vec)
  } else if (dim=="y") {
    dist.vec = abs(xy[2]-line$y)
    dist = min(dist.vec)
  }
  dist  
}

find.nearest.line.point = function(xy, line, dim="xy") {
  restore.point("find.nearests.line.point")
  
  if (line$type=="marker") {
    if (line$axis == "x") return(c(line$x[1],xy[2]))
    if (line$axis == "y") return(c(xy[1],line$y[1]))
  }

  if (dim=="xy") {
    dist.vec = (xy[1]-line$x)^2+(xy[2]-line$y)^2
  } else if (dim=="x") {
    dist.vec = abs(xy[1]-line$x)
  } else if (dim=="y") {
    dist.vec = abs(xy[2]-line$y)
  }
  row = which.min(dist.vec)
  c(x=line$x[row],y=line$y[row])
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

