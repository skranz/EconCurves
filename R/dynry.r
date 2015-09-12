
examples.dynry = function() {
  set.restore.point.options(display.restore.point = !TRUE)

  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  #es = load.story("ThreeEq_G_langfristig")
  #es = load.story("IS_LM_PC_G_kurzfristig")
  es = load.story("SimpleLabor3EqStory")
  init.story(es)
  par(mfrow=c(1,2),oma=c(0,0,0,0))

  em = es$em
  sim = em$sim
  dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)

}


init.dynry = function(es, em=es$em) {
  restore.point("init.dynry")
  
  es$t = es$step.num = 1
  es$wait.for.answer = FALSE
  es$tol = 0.07
  

  init.dynry.parts(es)
  
  
  init.model(em)
  if (is.null(es$scenario))
    es$scenario = em$scenarios[[es$scenarioId]]
  
  es$T = as.numeric(es$scenario$T)

  init.model.scen(em,scen = es$scenario)
  simulate.model(em,scen = es$scenario,init.scen = FALSE)
  
}

dynry.tparts = function(es,t=es$t) {
  row = which(es$tparts.df$start.t <= t & t <= es$tparts.df$end.t)
  es$tparts.df[row,]
}

dynry.next = function(es, t=es$t, step.num=es$step.num, update.es=TRUE) {
  restore.point("dynry.next")
  
  tparts = dynry.tparts(es,t)    

  if (step.num < tparts$num.parts) {
    step.num = step.num+1
  } else {
    if (t<es$T) {
      t = t+1
      step.num = 1
    } else {
      return(list(t=t, step.num=1, end=TRUE))
    }
  }
  
  if (update.es) {
    es$t = t
    es$step.num = step.num
  }
  return(list(t=t, step.num=step.num, end=FALSE))
}


dynry.prev = function(es, t=es$t, step.num=es$step.num, update.es=TRUE) {
  restore.point("dynry.prev")  

  start = FALSE
  if (step.num > 1) {
    step.num = step.num-1
  } else {
    if (t>1) {
      t = t-1
      tparts = dynry.tparts(es,t)    
      step.num = tparts$num.parts
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


dynry.forward = function(es, t = es$t, step.num = es$step.num, update.es=TRUE) {
  restore.point("dynry.forward")
  
  tparts = dynry.tparts(es,t)  
  if (tparts$row == NROW(es$tparts.df)) {
    t = es$T
    step.num = tparts$num.parts
  } else {
    row = tparts$row+1
    t = es$tparts.df$start.t[[row]]
    step.num = 1
  }
  if (update.es) {
    es$t = t
    es$step.num = step.num
  }
  
  return(list(t=t, step.num = step.num))
}


init.dynry.parts = function(es) {
  restore.point("init.dynry.parts")
  #period = es$periods[[2]]
  
  
  prev.part = list(t=0, shown=NULL)

  step.num = 0
  
  part.ind = 0
  
  part.names = names(es$parts)
  
  parts = vector("list",length(es$parts))

  i = 1
  hvals = list(t=1,section=NULL)

  while(TRUE) {
    if (i>length(es$parts)) break
    name = part.names[i]
    
    if (str.starts.with(name,"Period ")) {
      i = i+1
      hvals$t = as.numeric(str.right.of(name, "Period "))
      next
    }

    if (str.starts.with(name,"Section ")) {
      i = i +1
      hvals$section = str.right.of(name, "Section ")
      next
    }
    
    part = es$parts[[i]]
    part.ind = part.ind+1

    for (var in names(hvals)) {
      if (is.null(part[[var]])) {
        if (!is.null(hvals[[var]])) {
          part[[var]] = hvals[[var]]
        } else {
          part[[var]] = prev.part[[var]]
        }
      }
    }
    hvals = lapply(hvals, function(val) NULL)
    same.t = part$t == prev.part$t
    if (same.t) {
      step.num = step.num +1
    } else {
      step.num = 1
    }

    # If is part of the same period,
    # by default copy shown curves
    if (is.null(part$append)) {
      if (same.t)
        part$append = c("show")
    } else if (part$append[1]=="all") {
      part$append = c("show","tell")
    }
    part$shown = part$show    

    if ("show" %in% part$append)
      part$shown = unique(c(part$show, prev.part$shown))
    
        
    part$shown = setdiff(part$shown, part$hide)
    part$start.symbols = part$shown
    
    part$shown = unique(c(part$shown, story.part.task.symbols(part)))
    part$end.symbols = part$shown
    
    if ("tell" %in% part$append) {
      if (!is.null(prev.part$tell))
        part$tell = paste0(prev.part$tell, "\n", part$tell)
    }
      
    try(Encoding(part$tell) <- "UTF-8", silent=TRUE)
    try(Encoding(part$ask) <- "UTF-8", silent=TRUE)
    try(Encoding(part$success) <- "UTF-8", silent=TRUE)
      
    if (!is.null(part$task))
        part$task$type = get.story.part.task.type(part)
      
    parts[[part.ind]] = part
    names(parts)[part.ind] = name
    prev.part = part
    i = i+1
  }
  
  es$parts = parts[1:part.ind]
  
  t.vec = sapply(es$parts, function(part) part$t)  
  
  start.t = unique(t.vec)  
  end.t = c(start.t[-1]-1, Inf)
  parts = lapply(start.t, function(t) which(t.vec==t))
  num.parts = sapply(parts, function(ps) length(ps))
  
  es$tparts.df = data_frame(row=seq_along(start.t),start.t = start.t, end.t = end.t, parts = parts, num.parts=num.parts)

  es
}

get.dynry.part.ind = function(es, t=es$t, step.num = es$step.num) {
  row = which(es$tparts.df$start.t <= t & t <= es$tparts.df$end.t)
  es$tparts.df$part[[row]][step.num]
}


get.dynry.part = function(es, t=es$t, step.num = es$step.num) {
  restore.point("get.dynry.part")
  row = which(es$tparts.df$start.t <= t & t <= es$tparts.df$end.t)
  part.ind = es$tparts.df$parts[[row]][step.num]
  es$parts[[part.ind]]
}


get.dynry.step.symbols = function(es,t, step.num, solved=TRUE, previous.steps=TRUE) {
  part = get.dynry.part(es,t, step.num)
  
  if (!solved) {
    symbols = part$start.symbols
  } else if (solved) {
    symbols = part$end.symbols
  }
  symbols
}

dynry.step.dyplot = function(es, t, step, solved=FALSE, previous.steps=TRUE, pane.names = names(es$em$panes), vars = names(es$em$vars)) {
  restore.point("dynry.step.dyplot")
  
  em = es$em
  sim = em$sim
  sim = sim[,c("t",vars)]
  
  #if (t==1) return()
  
  if (t<NROW(sim)) {
    sim[(t+1):NROW(sim),vars]=NA 
  }
  symbols = get.dynry.step.symbols(es=es,t=t,step.num=step, solved=solved, previous.steps=previous.steps)

  t.vars = intersect(symbols, vars)
  hidden.vars = setdiff(vars, t.vars)
  if (length(hidden.vars)>0) {
    sim[t,hidden.vars] = NA
  }
  sim
  #cat("\nRefresh timeline: t=",t, " vars = ", paste0(t.vars, collapse=","))
  dyplot.timelines(sim,cols = vars,em = em)

}

get.dynry.step.lines = function(es, t, step.num, solved=FALSE, previous.steps=TRUE, pane.names = names(es$em$panes)) {
  restore.point("get.dynry.step.lines")
  part = get.dynry.part(es,t, step.num)
  
  if (!solved) {
    symbols = part$start.symbols
  } else if (solved) {
    symbols = part$end.symbols
  }
  compute.symbol.lines(t = t,em = es$em,symbols = symbols, pane.names=pane.names)
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
    part = get.dynry.part(es,t, step.num)
    task = part$task
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
  restore.point("check.find.answer")
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



