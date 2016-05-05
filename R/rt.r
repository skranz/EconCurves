
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
  if (is.null(ao$panes)) {
    ao$panes = list(ao$pane)
  }
  
  ao$panes = lapply(ao$panes, function(pane) {
    pane = get.pane.from.ps(pane = ao$pane, ps = ps,arg.li=ao)    
    pane$show = show
    # compute pane$data
    make.pane.data(pane=pane)
    pane
  })
  names(ao$panes) = sapply(ao$panes, function(pane) pane$name)
  
  ao$all.names = unique(unlist(lapply(ao$panes, function(pane) names(pane$objs))))
  
  # compute steps
  given_show = compute.show.list(show=ao$show,hide=ao$hide, data_rows=ao$data_rows, all.names = ao$all.names)

  # overwrite default show = ".all"
  ao$imgs = lapply(ao$panes, function(pane) {
    id = paste0("panequiz_",bi,"_",pane$name)
    list(
      id = id,
      html = tags$img(id=id, style="width: 5in; height:4in; cursor: pointer;", src="")
    )
  }) 

  lines = c(step.lines,length(txt)+1)
  steps = list("vector", length(step.lines))
  names(steps) = str.trim(str.right.of(txt[step.lines], "#. step "))

  
  i = 1  
  for (i in seq_along(steps)) {
    step = parse.hashdot.yaml(txt[(lines[i]+1):(lines[i+1]-1)])
    step$step.num = i
    step = panequiz.make.step(step, given_show=given_show, panes=ao$panes,ps=ps,bi=bi,ao=ao)
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
  menu = as.character(panequiz.menu.bar(bi=bi))
  ao$layout = HTML(replace.whiskers(ao$layout, list(text=textOutput,pane=paneOutput,menu=menu)))
  
  
  return(ao)
}

panequiz.ui.fun = function(ts,bi,...) {
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
  plot.html = HTML(step[[paste0(pmode,".plot")]][[1]]$html)
  setUI(ao$paneId,plot.html)  
}

panequiz.show.next.step = function(ts,...) {
  restore.point("panequiz.show.next.step")
  
  # first move to success
  if (ts$step.mode == "pre" | ts$step.mode == "failure") {
    if (ts$step.num > ts$sts$steps.solved) {
      panequiz.show.step(ts=ts,msg="You must solve the current task, before you can see the next task.")
    }
    ts$step.mode = "success"
    panequiz.show.step(ts=ts)
    return()
  }
    
  next.step.num = ts$step.num +1
  if (next.step.num > length(ts$ao$steps))
     return()
  
  ts$step.num = next.step.num
  ts$step.mode = "pre"
  panequiz.show.step(ts=ts)
}

panequiz.show.prev.step = function(ts, next.step.mode="pre",...) {
  restore.point("panequiz.show.prev.step")
  next.step.num = max(ts$step.num -1,1)
  ts$step.num = next.step.num
  ts$step.mode = next.step.mode   
  panequiz.show.step(ts=ts)
}



panequiz.make.step = function(step, panes=ao$panes, given_shows,ps,bi, ao, prev_shows=NULL, prev_src=NULL) {
  restore.point("panequiz.make.step")
  
  if (is.null(step$points)) step$points = 0
  if (is.null(step$score)) step$score = 0
  
  
  res = compute.step.shows(step=step, given_show=given_shows, all.names=ao$all.names, data_rows=ao$data_rows)
  step$preshow = res$preshow
  step$postshow = res$postshow


  step$pre.plot = step$post.plot = vector("list", length(panes))
  for (i in seq_along(panes)) {
    pane = panes[[i]]
    img.id = ao$imgs[[i]]$id
  
    filename = paste0("panequiz_",bi,"_",pane$name,"_pre_",step$step.num,".png")
    
    res = plot.to.html(plot.pane(pane,show = step$preshow, hide=NULL,data_rows=pane$data_rows), height.in=pane$height.in,width.in=pane$width.in,out.dir = ps$figure.dir,src.path = ps$figure.web.dir, filename=filename, embed=FALSE, compute.coordmap = TRUE, img.id = img.id,res=144,img.style = "cursor: pointer;")
    
    step$pre.plot[[i]] = res 
  
    filename = paste0("panequiz_",bi,"_",pane$name,"_post_",step$step.num,".png")
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
  
  step = panequiz.make.step.task(step=step, ps=ps,ao=ao) 
  
  step
}

panequiz.make.step.task = function(step,panes=ao$panes,ps,ao) {
  restore.point("panequiz.make.step.task")

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

panequiz.sol.txt.fun = panequiz.out.txt.fun = function(ts,solved=TRUE,...) {
  "\n--A pane quiz--\n"
}

panequiz.init.handlers = function(ao=ts$ao,ps=get.ps(), app=getApp(),ts=NULL,bi,...) {
  restore.point("panequiz.init.handlers")
  
  # NEED TO CORRECT
  pane = ao$panes[[1]]
  img.id = ao$imgs[[1]]$id
  
  imageClickHandler(img.id,fun = panequiz.click,ts=ts,pane=pane)  
  
  nextBtnId = paste0("panequizNextBtn_",bi)
  prevBtnId = paste0("panequizPrevBtn_",bi)
  buttonHandler(nextBtnId, panequiz.show.next.step, ts=ts)
  buttonHandler(prevBtnId, panequiz.show.prev.step, ts=ts)
  
  
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
    panequiz.show.step(ts=ts)
  } else {
    ts$step.mode = "failure"
    panequiz.show.step(ts=ts)
  }
  
  cat("\npixel = ",c(px,py)," xy = ",unlist(xy))
}

panequiz.menu.bar = function(bi) {
  restore.point("panequiz.menu.bar")
  
  nextBtnId = paste0("panequizNextBtn_",bi)
  prevBtnId = paste0("panequizPrevBtn_",bi)
  
  div(class="panequiz-menu-bar",
    HTML("<table width='100%'><tr>"),
    HTML("<td align='left' valign='center' nowrap>"),
    tagList(
      bsButton(prevBtnId,"<",size = "extra-small"),
      bsButton(nextBtnId,">",size = "extra-small")
    ),
    HTML("</td></tr></table>")
  )  
  
}


get.geom.name.and.row = function(id,data_rows=1, all.cols = names(data), data=NULL) {
  restore.point("get.geom.name.and.row")
  
  role.names = names(data_rows)
  if (is.null(role.names))
    role.names = as.character(data_rows)

  res.df = data_frame(name=id, row=data_rows[1])
  
  for (i in seq_along(role.names)) {
    rn = role.names[i]
    rcols = paste0(all.cols,"_",rn)
    rows = match(id,rcols)
    wrows = which(!is.na(rows))
    if (length(wrows)>0) {
      res.df$name[wrows] = all.cols[rows[wrows]]
      res.df$row[wrows] = data_rows[i]
    }
  }
  no.match = !res.df$name %in% all.cols
  if (any(no.match)) {
    stop("Could not find the field(s) ", paste(res.df$name[no.match], collapse=", ")," in the data.")
  }
  res.df 
}
whisker.values.from.data_rows = function(data, data_rows) {
  restore.point("whisker.values.from.data_rows")
  
  role.names = names(data_rows)
  if (is.null(role.names))
    role.names = as.character(data_rows)
  all.cols = colnames(data)
  val0 = as.list(data[data_rows[1],])
  val.li = lapply(seq_along(data_rows), function(i) {
    vals = as.list(data[data_rows[i],])
    names(vals) = paste0(all.cols,"_", role.names[i])
    vals
  })
  c(val0, do.call(c,val.li))
   
  
}



replace.data_rows.whiskers = function(txt, data=pane$data, data_rows=pane$data_rows, pane=NULL) {
  restore.point("panequiz.replace.whiskers")
  values = whisker.values.from.data_rows(data=data, data_rows=data_rows)
  replace.whiskers(txt, values)
}

set_image_src_js = function() {
"  
// change the src of an img without flicker
function set_image_src(src,id) {
  // create a new Image object
  var imgvar = new Image();
  // when preload is complete, apply the image to the img object
  imgvar.onload = function() {
    ('#'+id).src=src;
  };
  imgvar.src = src;
}
"
}