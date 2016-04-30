
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
    ui.fun = function(...) {
      panequiz.ui.fun(...)
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
  show = ao$show
  ao$pane = pane = get.pane.from.ps(pane = ao$pane, ps = ps,arg.li=ao)
  # overwrite default show = ".all"
  ao$pane$show = show
  
  ao$pane$img.id = paste0("panequiz_",bi,"_",ao$pane$name)
  
  lines = c(step.lines,length(txt)+1)
  steps = list("vector", length(step.lines))
  names(steps) = str.trim(str.right.of(txt[step.lines], "#. step "))

  # compute pane$data
  make.pane.data(pane=pane)
  
  # compute steps
  given_show = compute.show.list(show=pane$show,hide=pane$show, data_rows=pane$data_rows, pane=pane)
  
  i = 1  
  for (i in seq_along(steps)) {
    step = parse.hashdot.yaml(txt[(lines[i]+1):(lines[i+1]-1)])
    step$step.num = i
    step = make.panequiz.step(step, given_show=given_show, pane=ao$pane,ps=ps,bi=bi)
    steps[[i]] = step
    given_show = step$postshow      
  }

  ao$steps = steps
  ao$max.points = sum(sapply(ao$steps, function(step) step$points))
  
  # init layout
  if (is.null(ao$layout)) {
    ao$layout = '
    <table><tr>
    <td>{{text}}</td>
    <td>{{pane}}</td>
    </tr></table>
    '    
  }
  ao$textId = paste0("panequiz_text_",bi)
  ao$paneId = paste0("panequiz_pane_",bi)
  
  textOutput = as.character(uiOutput(ao$textId))
  paneOutput = as.character(uiOutput(ao$paneId))  
  ao$layout = HTML(replace.whiskers(ao$layout, list(text=textOutput,pane=paneOutput)))
  
  
  return(ao)
}

panequiz.ui.fun = function(ts,...) {
  restore.point("pane.quiz.ui.fun")
  ao = ts$ao
  ui = ao$layout
  panequiz.show.step(ts=ts)
  ui
}

panequiz.show.step = function(step.num=ts$step.num, step=ao$steps[[step.num]], ao=ts$ao, step.mode = ts$step.mode, ts=NULL, msg="") {
  restore.point("panequiz.show.step")
  
  # show text
  html = p(step[[paste0(step.mode,".html")]])
  if (nchar(msg)>0) {
    html = div(html,p(msg))
  }
  setUI(ao$textId,html)
  
  # show pane figure
  pmode = step.mode
  if (step.mode=="failure" | step.mode=="pre") {
    pmode = "pre"
  } else {
    pmode = "post"    
  }
  plot.html = HTML(step[[paste0(pmode,".plot")]]$html)
  setUI(ao$paneId,plot.html)  
}

panequiz.show.next.step = function(ts) {
  restore.point("panequiz.show.next.step")
  next.step.num = ts$step.num +1
  if (next.step.num > length(ts$ao$steps)) return()
  
  if (next.step.num > ts$sts$steps.solved +1) {
    panequiz.show.step(ts,msg="You must solve the current task, before you can see the next task.")
    return()
  }
  ts$step.num = next.step.num
  ts$step.mode = "pre"
  panequiz.show.step(ts=ts)
}

panequiz.show.prev.step = function(ts=NULL, next.step.mode="pre") {
  restore.point("panequiz.show.prev.step")
  next.step.num = max(ts$step.num -1,1)
  ts$step.mode = next.step.mode   
  panequiz.show.step(ts=ts)
}



make.panequiz.step = function(step, pane, given_shows,ps,bi) {
  restore.point("make.panequiz.step")
  
  if (is.null(step$points)) step$points = 0
  if (is.null(step$score)) step$score = 0
  
  
  res = compute.step.shows(step=step, given_show=given_shows, pane=pane)
  step$preshow = res$preshow
  step$postshow = res$postshow


  img.id = pane$img.id

  filename = paste0("panequiz_",bi,"_",pane$name,"_pre_",step$step.num,".png")
  
  res = plot.to.html(plot.pane(pane,show = step$preshow, hide=NULL,data_rows=pane$data_rows), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144,img.style = "cursor: pointer;")
  
  step$pre.plot = res 

  filename = paste0("panequiz_",bi,"_",pane$name,"_post_",step$step.num,".png")
  res = plot.to.html(plot.pane(pane,show = step$postshow, hide=NULL,data_rows=pane$data_rows), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144,img.style = "cursor: pointer;")
  
  step$post.plot = res 
  
  pre.html = md2html(step$tell)
  
  step$pre.html = withMathJax(HTML(pre.html))
  step$success.html = withMathJax(HTML(paste0(pre.html, md2html(step$success))))
  if (is.null(step$failure)) step$failure = ""
  step$failure.html = withMathJax(HTML(paste0(pre.html, md2html(step$failure))))
  
  
  step = panequiz.make.step.task(step=step, pane=pane, ps=ps) 
  
  step
}

panequiz.make.step.task = function(step,pane,ps) {
  restore.point("panequiz.make.step.task")

  task.fields = c("find","find_shift")
  step$task.type = task.fields[task.fields %in% names(step)]
  if (length(step$task.type)==0) step$task.type = "none"
  step$has.task = step$task.type != "none"
  if (!step$has.task) return(step)
  
  step$task.fun = function(xy,...) {
    list(ok=TRUE)
  }
  return(step)
}

panequiz.make.org.task.state = function(ao,...) {
  list(
    solved=FALSE,
    step.num = 1,
    step.mode = "pre",
    points = 0,
    score = 0,
    sts = list(steps.solved = 0),
    ao=ao
  )
}

panequiz.init.task.state.with.ups = function(ts,ups, task.ind=ts$task.ind,...) {
  restore.point("panequiz.init.task.state.with.ups")
  
  ts$sts = ups$sts[[task.ind]]
  if (is.null(ts$sts)) {
    warning("ups$sts is null")
    return(panequiz.init.task.state.without.ups(ts=ts, task.ind=task.ind,...))
  }
  ts$step.num = max(1,min(ts$sts$steps.solved, length(ts$ao$steps)))
  ts$step.mode = if (ts$step.num > ts$sts$steps.solved) "pre" else "success"
  
  ts$solved = ups$utt$was.solved[task.ind]
  
  ts
}

panequiz.init.task.state.without.ups = function(ts,ups, task.ind=ts$task.ind,...) {
  restore.point("panequiz.init.task.state.without.ups")
  if (is.null(ts$sts)) ts$sts = list(steps.solved = 0)
  
  ts
}


panequiz.shown.txt.fun = function(ts,solved=FALSE,...) {
  "\n--A pane quiz--\n"
} 

panequiz.sol.txt.fun = function(ts,solved=TRUE,...) {
  "\n--A pane quiz--\n"
}

panequiz.init.handlers = function(ao=ts$ao,ps=get.ps(), app=getApp(),ts=NULL,...) {
  restore.point("panequiz.init.handlers")
  
  pane = ao$pane
  imageClickHandler(pane$img.id,fun = panequiz.click,ts=ts,pane=pane)  
}

panequiz.click = function(x,y,...,ts=NULL,pane=NULL) {
  args = list(...)
  restore.point("panequiz.click")
  cat("panequiz.click")
  
  ao = ts$ao
  step = ao$steps[[ts$step.num]]
  step.mode = ts$step.mode

  if (step.mode == "success" | !step$has.task) {
    panequiz.show.next.step(ts=ts)
    return()
  }
  
  # translate pixel to plot coordinates
  coordmap = step$pre.plot$coordmap
  px = x; py = y
  xy = scaleInvCoords(px,py,coordmap)
  
  res = step$task.fun(xy=xy)
  
  # task correctly solved
  if (res$ok) {
    was.solved = ts$sts$steps.solved >= step$step.num
    ts$step.mode = "success"
    if (!was.solved) {
      ts$sts$steps.solved = step$step.num
      ts$points = ts$points + step$points
      ts$score = ts$score + step$score
    }
    panequiz.show.step(ts=ts)
  } else {
    ts$step.mode = "failure"
    panequiz.show.step(ts=ts)
  }
  
  cat("\npixel = ",c(px,py)," xy = ",unlist(xy))
}

panequiz.menu.bar = function(ao) {
  div(class="panequiz-menu-bar",
    HTML("<table width='100%'><tr>"),
    HTML("<td align='right' valign='center' nowrap>"),
    tagList(
      bsButton("panequizPrevBtn","<",size = "extra-small"),
      bsButton("panequizNextBtn",">",size = "extra-small"),
    ),
    HTML("</td></tr></table>")
  )  

}
