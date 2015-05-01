
examples.console.story = function() {
  set.restore.point.options(display.restore.point = !TRUE)
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  es = load.story("ThreeEq_G_kurzfristig")
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


tell.story.on.console = function(es, t.start=1, step.start=1, mfrow=c(1,1), ask=FALSE, num.attempts=3) {
  restore.point("tell.story.on.console")
  
  es$mfrow = mfrow
  
  par(mfrow=mfrow)
  
  t = 3
  for (t in t.start:es$T) {
    period = get.story.period(es=es,t=t)
    if (t>t.start) step.start=1

    for (s in int.seq(step.start,length(period$steps))) {
      step = period$steps[[s]]
      cat(paste0("\n",t,".", s,":"))
      tell.step.task.on.console(es,t,s)
      story.step.dyplot(es=es,t=t,step = s,solved = FALSE,vars = es$timelineVars)
      
      if (ask & length(step$task)>0) {
        ret = ask.task.on.console(es, t, s, num.attempts=num.attempts)
        if (!ret)
          cat("Let us just proceed as if you were correct. You can try next time.\n")
      } else {
        readline(prompt="[Press Enter to continue] [Esc to stop]")
      }
      if (length(step$task)>0) {
        story.step.dyplot(es=es,t=t,step = s,solved = TRUE,vars = es$timelineVars)
        tell.step.sol.on.console(es,t,s)
        
        #Sys.sleep(1)
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
  
  periods = get.story.period(es,t=t)
  st = period$steps[[step]]

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
    xy = locator(1)
    ret = check.click.answer(es=es,xy = xy,pane.name = pane.name,t = t,step.num = step,task = task)
    if (ret) break
    if (!ret & attempt<num.attempts)
      cat("\nSorry that was wrong. Try again.")
    if (!ret & attempt<num.attempts)
      cat("\nSorry, that was ", attempt , "times wrong.")
  }
  
  par(mfrow=es$mfrow)
  return(ret)
  
}
