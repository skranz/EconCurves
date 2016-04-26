
rtutor.addon.pane = function() {
  list(
    package = "EconCurves",
    type = "pane",
    mode = "block",
    need.task.env = FALSE,
    change.task.env = FALSE,
    is.task = FALSE,
    is.static = TRUE,
    parse.fun = function(inner.txt,type="pane",name=args$name,id=paste0("addon__",type,"__",name),args=NULL, bdf=NULL, task.ind=NULL,...) {
      restore.point("pane.parse.fun")
      
      pane = init.yaml.pane(yaml=merge.lines(inner.txt), direct=TRUE, name=name)
      pane
  }
  )
}

rtutor.addon.plotpane = function() {
  list(
    package = "EconCurves",
    type = "pane",
    mode = "block",
    need.task.env = FALSE,
    change.task.env = FALSE,
    is.task = FALSE,
    is.static = TRUE,
    parse.fun = function(inner.txt,type="pane",name=args$name,id=paste0("addon__",type,"__",name),args=NULL, task.ind=NULL,ps,bdf=ps$bdf,bi,...) {
      restore.point("showpane.parse.fun")
      yaml = merge.lines(inner.txt)
      arg.li = read.yaml(text=yaml)
      if (is.null(arg.li$pane)) arg.li$pane = name
      pane = get.pane.from.ps(pane = arg.li$pane, ps = ps,arg.li=arg.li)
      
      filename = paste0("plotpane_",bi,".png")
      img.id = paste0(ps$ps.id,"_plotpane_",bi)
      res = plot.to.html(plot.pane(pane), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144)
      c(list(img.id=img.id),res)
    },
    ui.fun = function(ao,...) {
      HTML(ao$html)
    }
  )
}

get.pane.from.ps = function(pane=arg.li$pane, ps, bdf = ps$bdf, arg.li = NULL) {
  restore.point("get.pane.from.ps")
  if (is.character(pane)) {
    pane.name = pane
    row = which(bdf$stype=="pane" & bdf$name==pane)[1]
    if (is.na(row)) {
      stop(paste0("You want to plot the pane ", name, " but have not defined that pane before"))
    }
    pane = bdf$obj[[row]]$ao
    pane$pane.name = pane.name
  } else {
    pane = init.yaml.pane(pane=arg.li$pane)        
  }
  if (is.null(pane$width.in)) pane$width.in = 5
  if (is.null(pane$height.in)) pane$height.in = 4
  cols = setdiff(names(arg.li),c("name","pane","height.in","width.in","params"))
  for (col in cols) {
    pane[[col]] = arg.li[[col]]
  }
  if (is.null(pane$params)) pane$params = list()
  pane$params[names(arg.li$params)] = arg.li$params
  if (any(c("params","datavar") %in% names(arg.li)) | isTRUE(pane$use.dataenv) ) {
    pane$dataenv = ps$preenv
    make.pane.data(pane=pane) 
  }
      
  pane
}

rtutor.addon.panequiz = function() {
  list(
    package = "EconCurves",
    type = "pane",
    mode = "block",
    need.task.env = FALSE,
    change.task.env = FALSE,
    is.task = TRUE,
    is.static = FALSE,
    parse.fun = function(...) {panequiz.parse.fun(...)},
    ui.fun = function(ao,...) {
      HTML("Panequiz here...")
    },
    make.org.task.state = panequiz.make.org.task.state,
    init.task.state.with.ups = panequiz.init.task.state.with.ups,
    init.task.state.without.ups = panequiz.init.task.state.without.ups,
    init.handlers = panequiz.init.handlers,
    shown.txt.fun = panequiz.shown.txt.fun,
    sol.txt.fun = panequiz.sol.txt.fun,
    out.txt.fun = panequiz.sol.txt.fun
  )
}


from_to = function(from, to) {
  if (to < from) return(NULL)
  from:to
}

panequiz.parse.fun = function(inner.txt,type="panequiz",name=args$name,id=paste0("addon__",type,"__",name),args=NULL, task.ind=NULL,ps,bdf=ps$bdf,bi,...) {
  restore.point("panequiz.parse.fun")
  
  txt = inner.txt
  step.lines = which(str.starts.with(txt,"#. step"))
  if (length(step.lines)==0) {
    stop("You must define at least one step in each panequiz.")
  }
  start.lines = from_to(1,step.lines[1]-1)
  ao = list()
  if (length(start.lines)>0) {
    ao = parse.hashdot.yaml(txt[start.lines])
  }
  ao$pane = pane = get.pane.from.ps(pane = ao$pane, ps = ps,arg.li=ao)
  
  lines = c(step.lines,length(txt)+1)
  steps = list("vector", length(step.lines))
  names(steps) = str.trim(str.right.of(txt[step.lines], "#. step "))

  # compute steps
  given_show = compute.show.list(show=pane$show,hide=pane$show, data_rows=pane$data_rows, pane=pane)
    
  for (i in seq_along(steps)) {
    step = parse.hashdot.yaml(txt[(lines[i]+1):(lines[i+1]-1)])
    step$step.num = i
    step = make.panequiz.step(step, given_show=given_show, pane=ao$pane,ps=ps)
    steps[[i]] = step
    given_show = step$postshow      
  }

  ao$steps = steps
  ao$points = sum(sapply(ao$steps, function(step) step$points))

  return(ao)
}

make.panequiz.step = function(step, pane, given_shows,ps) {
  restore.point("make.panequiz.step")
  
  if (is.null(step$points)) steps$points = 0
  res = compute.step.shows(step=step, given_show=given_shows, pane=pane)
  step$preshow = res$preshow
  step$postshow = res$postshow

  img.id = paste0("panequiz_",bi,"_step_",step$step.num)

    
  filename = paste0("panequiz_",bi,"_pre_step_",step$step.num,".png")
  res = plot.to.html(plot.pane(pane,show = step$preshow, hide=NULL), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144)
  
  pre = res 

  filename = paste0("panequiz_",bi,"_post_step_",step$step.num,".png")
  res = plot.to.html(plot.pane(pane,show = step$postshow, hide=NULL), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144)
  
  post = res 

  
  step
  
}

panequiz.make.org.task.state = function(ao,...) {
  list(
    step = 1,
    sts = list(steps.solved = 0),
    ao=ao
  )
}

panequiz.init.task.state.with.ups = function(ts,ups, task.ind=ts$task.ind,...) {
  ts$sts = ups$sts[[task.ind]]
  ts$step = min(ts$sts$steps.solved+1, ts$ao$num.steps)
  ts$solved = ups$utt$was.solved[task.ind]
  ts
}

panequiz.init.task.state.without.ups = function(ts,ups, task.ind=ts$task.ind,...) {
  ts
}


panequiz.shown.txt.fun = function(ts,solved=FALSE,...) {
  "\n--A pane quiz--\n"
} 

panequiz.sol.txt.fun = function(ts,solved=TRUE,...) {
  "\n--A pane quiz--\n"
}

panequiz.init.handlers = function(ao=ts$ao,ps=get.ps(), app=getApp(),ts=NULL,...) {
  
}
