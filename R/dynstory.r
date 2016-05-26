
rtutor.addon.dynstory = function() {
  list(
    package = "EconCurves",
    type = "pane",
    mode = "block",
    need.task.env = FALSE,
    change.task.env = FALSE,
    is.task = TRUE,
    is.static = FALSE,
    parse.fun = function(...) {dynstory.parse.fun(...)},
    ui.fun = function(...) {
      dynstory.ui.fun(...)
    },
    make.org.task.state = dynstory.make.org.task.state,
    init.task.state.with.ups = dynstory.init.task.state.with.ups,
    init.task.state.without.ups = dynstory.init.task.state.without.ups,
    init.handlers = dynstory.init.handlers,
    shown.txt.fun = dynstory.shown.txt.fun,
    sol.txt.fun = dynstory.sol.txt.fun,
    out.txt.fun = dynstory.sol.txt.fun
  )
}


from_to = function(from, to) {
  if (to < from) return(NULL)
  from:to
}

dynstory.parse.fun = function(inner.txt,type="dynstory",name=args$name,id=paste0("addon__",type,"__",name),args=NULL, task.ind=NULL,ps,bdf=ps$bdf,bi,...) {
  restore.point("dynstory.parse.fun")
  
  txt = inner.txt
  step.lines = which(str.starts.with(txt,"#. step"))
  if (length(step.lines)==0) {
    stop("You must define at least one step in each dynstory.")
  }
  start.lines = from_to(1,step.lines[1]-1)
  ao = list()
  if (length(start.lines)>0) {
    ao = parse.hashdot.yaml(txt[start.lines])
  }
  show = ao$show
  if (is.null(ao$panes)) {
    ao$panes = list(ao$pane)
  }
  
  ao$panes = lapply(ao$panes, function(pane) {
    pane = get.pane.from.ps(pane = ao$pane, ps = ps,arg.li=ao)
    pane$show = show
    pane
  })
  names(ao$panes) = sapply(ao$panes, function(pane) pane$name)
  
  ao$all.names = unique(unlist(lapply(ao$panes, function(pane) names(pane$objs))))
  
  # compute steps
  given_show = compute.show.list(show=ao$show,hide=ao$hide, data_rows=ao$data_rows, all.names = ao$all.names)

  # overwrite default show = ".all"
  ao$imgs = lapply(ao$panes, function(pane) {
    id = paste0("dynstory_",bi,"_",pane$name)
    list(
      id = id,
      html = tags$img(id=id, style="width: 5in; height:4in; cursor: pointer;", src="")
    )
  }) 

  lines = c(step.lines,length(txt)+1)
  steps = vector("list", length(step.lines))
  names(steps) = str.trim(str.right.of(txt[step.lines], "#. step "))

  
  i = 1  
  for (i in seq_along(steps)) {
    step = parse.hashdot.yaml(txt[(lines[i]+1):(lines[i+1]-1)])
    step$step.num = i
    step = dynstory.make.step(step, given_show=given_show, panes=ao$panes,ps=ps,bi=bi,ao=ao)
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
  
  
  ao$textId = paste0("dynstory_text_",bi)
  ao$paneId = paste0("dynstory_pane_",bi)
  
  textOutput = as.character(uiOutput(ao$textId))
  paneOutput = as.character(uiOutput(ao$paneId))  
  menu = as.character(dynstory.menu.bar(bi=bi))
  ao$layout = HTML(replace.whiskers(ao$layout, list(text=textOutput,pane=paneOutput,menu=menu)))
  
  
  return(ao)
}

dynstory.ui.fun = function(ts,bi,...) {
  restore.point("pane.quiz.ui.fun")
  ao = ts$ao
  ui = ao$layout
  

  dynstory.show.step(ts=ts)
  ui
}

dynstory.show.step = function(step.num=ts$step.num, step=ao$steps[[step.num]], ao=ts$ao, step.mode = ts$step.mode, ts=NULL, msg="") {
  restore.point("dynstory.show.step")
  
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
  plot.html = HTML(step[[paste0(pmode,".plot")]][[1]]$html)
  setUI(ao$paneId,plot.html)  
}

dynstory.show.next.step = function(ts,...) {
  restore.point("dynstory.show.next.step")
  
  # first move to success
  if (ts$step.mode == "pre" | ts$step.mode == "failure") {
    if (ts$step.num > ts$sts$steps.solved) {
      dynstory.show.step(ts=ts,msg="You must solve the current task, before you can see the next task.")
    }
    ts$step.mode = "success"
    dynstory.show.step(ts=ts)
    return()
  }
    
  next.step.num = ts$step.num +1
  if (next.step.num > length(ts$ao$steps))
     return()
  
  ts$step.num = next.step.num
  ts$step.mode = "pre"
  dynstory.show.step(ts=ts)
}

dynstory.show.prev.step = function(ts, next.step.mode="pre",...) {
  restore.point("dynstory.show.prev.step")
  next.step.num = max(ts$step.num -1,1)
  ts$step.num = next.step.num
  ts$step.mode = next.step.mode   
  dynstory.show.step(ts=ts)
}



dynstory.make.step = function(step, panes=ao$panes, given_shows,ps,bi, ao, prev_shows=NULL, prev_src=NULL) {
  restore.point("dynstory.make.step")
  
  if (is.null(step$points)) step$points = 0
  if (is.null(step$score)) step$score = 0
  
  
  res = compute.step.shows(step=step, given_show=given_shows, all.names=ao$all.names, data_rows=ao$data_rows)
  step$preshow = res$preshow
  step$postshow = res$postshow


  step$pre.plot = step$post.plot = vector("list", length(panes))
  for (i in seq_along(panes)) {
    pane = panes[[i]]
    img.id = ao$imgs[[i]]$id
  
    filename = paste0("dynstory_",bi,"_",pane$name,"_pre_",step$step.num,".png")
    
    res = plot.to.html(plot.pane(pane,show = step$preshow, hide=NULL,data_rows=pane$data_rows), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144,img.style = "cursor: pointer;")
    
    step$pre.plot[[i]] = res 
  
    filename = paste0("dynstory_",bi,"_",pane$name,"_post_",step$step.num,".png")
    res = plot.to.html(plot.pane(pane,show = step$postshow, hide=NULL,data_rows=pane$data_rows), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144,img.style = "cursor: pointer;")
    
    step$post.plot[[i]] = res 
  }
  
  whisker.values = whisker.values.from.data_rows(data=pane$data, data_rows=pane$data_rows)
  
  pre.html = replace.whiskers(md2html(step$tell), whisker.values)
  success.html = replace.whiskers(md2html(step$success), whisker.values)
  if (is.null(step$failure)) step$failure = "---\nNot yet correct!"
  failure.html = replace.whiskers(md2html(step$failure), whisker.values)
  
  step$pre.html = withMathJax(HTML(pre.html))
  step$success.html = withMathJax(HTML(paste0(pre.html, success.html)))
  step$failure.html = withMathJax(HTML(paste0(pre.html,failure.html)))
  
  step = dynstory.make.step.task(step=step, ps=ps,ao=ao) 
  
  step
}

dynstory.make.step.task = function(step,panes=ao$panes,ps,ao) {
  restore.point("dynstory.make.step.task")

  data_rows = ao$data_rows
  # NEED TO CORRECT
  pane = panes[[1]]
  
  task.fields = c("find","find_shift")
  step$task.type = task.type = task.fields[task.fields %in% names(step)]
  if (length(step$task.type)==0) step$task.type = "none"
  step$has.task = step$task.type != "none"
  if (!step$has.task) return(step)
  

  task.fun.env = new.env(parent=globalenv())

  # find geom that are specified in the task
  nr = get.geom.name.and.row(step[[task.type]],data_rows, data=pane$data)
  if (task.type == "find") {
    restore.point("make.find.task")
    
    task.fun.env$geom = pane$geoms.li[[nr$row]][[nr$name]]
    task.fun = function(xy,pane.name=NULL,...) {
      ok = is.point.on.geom(xy,geom)
      list(ok=ok)
    }
  } else if (task.type == "find_shift") {
    restore.point("make.find.shift.task")
    
    if (NROW(nr)==1) {
      if (nr$row != data_rows[1]) stop("If you specify only a single symbol name in find_shift it must correspond to the first data_row")
      
      source.geom = pane$geoms.li[[data_rows[1]]][[nr$name]]
      dest.geom = pane$geoms.li[[data_rows[2]]][[nr$name]]
      
    } else {
      source.geom = pane$geoms.li[[nr$row[1] ]][[nr$name[1] ]]
      dest.geom = pane$geoms.li[[nr$row[2] ]][[nr$name[2] ]]
    }
    ggpos = geom.to.geom.pos(new=dest.geom, old=source.geom)
    
    task.fun.env$source.geom = source.geom
    task.fun.env$ggpos = ggpos
    task.fun = function(xy,pane.name = NULL,...) {
      pgpos = point.to.geom.pos(xy, source.geom)
      ok = length(setdiff(ggpos,pgpos))==0
      list(ok=ok)
    }
  } else {
    task.fun = function(xy,...) {
      list(ok=TRUE)
    }
  }
  environment(task.fun) = task.fun.env
  step$task.fun = task.fun
  return(step)
}



dynstory.make.org.task.state = function(ao,...) {
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

dynstory.init.task.state.with.ups = function(ts,ups, task.ind=ts$task.ind,...) {
  restore.point("dynstory.init.task.state.with.ups")
  
  ts$sts = ups$sts[[task.ind]]
  if (is.null(ts$sts)) {
    warning("ups$sts is null")
    return(dynstory.init.task.state.without.ups(ts=ts, task.ind=task.ind,...))
  }
  ts$step.num = max(1,min(ts$sts$steps.solved, length(ts$ao$steps)))
  ts$step.mode = if (ts$step.num > ts$sts$steps.solved) "pre" else "success"
  
  ts$solved = ups$utt$was.solved[task.ind]
  
  ts
}

dynstory.init.task.state.without.ups = function(ts,ups, task.ind=ts$task.ind,...) {
  restore.point("dynstory.init.task.state.without.ups")
  if (is.null(ts$sts)) ts$sts = list(steps.solved = 0)
  
  ts
}


dynstory.shown.txt.fun = function(ts,solved=FALSE,...) {
  "\n--A pane quiz--\n"
} 

dynstory.sol.txt.fun = dynstory.out.txt.fun = function(ts,solved=TRUE,...) {
  "\n--A pane quiz--\n"
}

dynstory.init.handlers = function(ao=ts$ao,ps=get.ps(), app=getApp(),ts=NULL,bi,...) {
  restore.point("dynstory.init.handlers")
  
  # NEED TO CORRECT
  pane = ao$panes[[1]]
  img.id = ao$imgs[[1]]$id
  
  imageClickHandler(img.id,fun = dynstory.click,ts=ts,pane=pane)  
  
  nextBtnId = paste0("dynstoryNextBtn_",bi)
  prevBtnId = paste0("dynstoryPrevBtn_",bi)
  buttonHandler(nextBtnId, dynstory.show.next.step, ts=ts)
  buttonHandler(prevBtnId, dynstory.show.prev.step, ts=ts)
  
  
}

dynstory.click = function(x,y,...,ts=NULL,pane=NULL) {
  args = list(...)
  restore.point("dynstory.click")
  cat("dynstory.click")
  
  ao = ts$ao
  step = ao$steps[[ts$step.num]]
  step.mode = ts$step.mode

  if (step.mode == "success" | !step$has.task) {
    dynstory.show.next.step(ts=ts)
    return()
  }
  
  # translate pixel to plot coordinates
  ## NEED TO CHANGE FOR MULTIPLE PANES
  coordmap = step$pre.plot[[1]]$coordmap
  px = x; py = y
  xy = unlist(scaleInvCoords(px,py,coordmap))
  
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
    dynstory.show.step(ts=ts)
  } else {
    ts$step.mode = "failure"
    dynstory.show.step(ts=ts)
  }
  
  cat("\npixel = ",c(px,py)," xy = ",unlist(xy))
}

dynstory.menu.bar = function(bi) {
  restore.point("dynstory.menu.bar")
  
  nextBtnId = paste0("dynstoryNextBtn_",bi)
  prevBtnId = paste0("dynstoryPrevBtn_",bi)
  
  div(class="dynstory-menu-bar",
    HTML("<table width='100%'><tr>"),
    HTML("<td align='left' valign='center' nowrap>"),
    tagList(
      bsButton(prevBtnId,"<",size = "extra-small"),
      bsButton(nextBtnId,">",size = "extra-small")
    ),
    HTML("</td></tr></table>")
  )  
  
}

