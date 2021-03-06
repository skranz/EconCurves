
rtutor.addon.pane = function() {
  list(
    package = "EconCurves",
    type = "pane",
    mode = "block",
    need.task.env = FALSE,
    change.task.env = FALSE,
    is.task = FALSE,
    is.static = TRUE,
    parse.fun = function(inner.txt,type="pane",name=args$name,id=paste0("addon__",type,"__",name),args=NULL, bdf=NULL, task.ind=NULL,ps = get.ps(),...) {
      restore.point("pane.parse.fun")
      pane = init.yaml.pane(yaml=merge.lines(inner.txt), direct=TRUE, name=name)
      make.pane.data(pane=pane, dataenv=ps$pre.env)
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
      restore.point("plotpane.parse.fun")
      yaml = merge.lines(inner.txt)
      arg.li = read.yaml(text=yaml)
      if (is.null(arg.li$pane)) arg.li$pane = name
      if (is.null(arg.li[["pane"]])) {
        stop("You have not specified a pane name in your plotpane block.")
      }
      pane = get.pane.from.ps(pane = arg.li$pane, ps = ps,arg.li=arg.li, shallow.copy = TRUE)
      img.id = paste0(ps$ps.id,"_plotpane_",bi)
      res = pane.svg(pane,id=img.id)

      c(list(img.id=img.id),res)
    },
    init.handlers = function(wid,...) {
      restore.point("plotpane init.handlers")
      
      svgClickHandler(wid$img.id,stop.propagation = FALSE,fun = function(x,y,...) {
        restore.point("plotpane click.handler")
      

        cat("\n click x = ",range.to.domain(x=x,svg=wid$svg) ," y = ",range.to.domain(y=y,svg=wid$svg),"\n")
      })
    },
    ui.fun = function(wid,...) {
      restore.point("plotpane.ui.fun")
      #addResourcePath("econcurves",paste0(path.package("EconCurves"),"/www"))
      tagList(
        #singleton(tags$script(src="econcurves/comic.min.js")),
        HTML(wid$html)
        #tags$script(paste0("COMIC.magic([ document.getElementById('",wid$img.id,"')]);"))
      )
    }
  )
}

get.pane.from.ps = function(pane=arg.li$pane, ps, bdf = ps$bdf, arg.li = NULL, shallow.copy=TRUE) {
  restore.point("get.pane.from.ps")
  if (is.character(pane)) {
    pane.name = pane
    row = which(bdf$stype=="pane" & bdf$name==pane)[1]
    if (is.na(row)) {
      stop(paste0("You want to plot the pane ", pane.name, " but have not defined that pane before"))
    }
    pane = bdf$obj[[row]]$wid
    pane$pane.name = pane.name
    if (shallow.copy)
      pane = as.environment(as.list(pane))
  } else {
    pane = init.yaml.pane(pane=arg.li$pane)        
  }
  #if (is.null(pane$width.in)) pane$width.in = 5
  #if (is.null(pane$height.in)) pane$height.in = 4
  cols = setdiff(names(arg.li),c("name","pane","height.in","width.in","params","curves","points","objects"))
  
  arg.li$xrange = unlist(arg.li$xrange)
  arg.li$yrange = unlist(arg.li$yrange)

  pane = copy.into.nested.list(pane, arg.li[cols])

  if (!is.null(arg.li$xrange)) pane$data_xrange=FALSE
  if (!is.null(arg.li$yrange)) pane$data_yrange=FALSE
  
    
  pane = update.pane.objs(pane=pane, curves=arg.li[["curves"]], points=arg.li[["points"]], xmarkers=arg.li[["xmarkers"]], ymarkers=arg.li[["ymarkers"]], objects = arg.li[["objects"]])
  
  if (is.null(pane$params)) pane$params = list()
  pane$params[names(arg.li$params)] = arg.li$params
  if (any(c("params","datavar","data") %in% names(arg.li)) | isTRUE(pane$use.dataenv) ) {
    if (!is.null(arg.li$datavar) | !is.null(arg.li$data))
      pane$data = NULL
    pane$dataenv = ps$pre.env
    make.pane.data(pane=pane) 
  }
  
  if (!is.null(arg.li$zoom)) {
    pane$width = round(pane$org.width*arg.li$zoom)
    pane$height = round(pane$org.height*arg.li$zoom)
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

panequiz.parse.fun = function(inner.txt,type="panequiz",name=args$name,id=paste0("addon__",type,"__",name),args=NULL, task.ind=NULL,ps,bdf=ps$bdf,bi,on.tol = 0.04,...) {
  restore.point("panequiz.parse.fun")
  
  txt = inner.txt
  step.lines = which(str.starts.with(txt,"#. step"))
  if (length(step.lines)==0) {
    stop("You must define at least one step in each panequiz.")
  }
  start.lines = from_to(1,step.lines[1]-1)
  wid = list()
  if (length(start.lines)>0) {
    wid = parse.hashdot.yaml(txt[start.lines])
  }
  wid$org.list = wid
  
  wid$id = paste0("panequiz_",bi)

  if (!is.null(wid[["continue"]])) {
    pre.name = wid$continue
    row = which(bdf$stype=="panequiz" & bdf$name==pre.name)[1]
    if (is.na(row)) {
      stop(paste0("Your panequiz states to continute the panequiz ", pre.name, " but I could not find that pnaequiz."))
    }
    
    pre = bdf$obj[[row]]$wid
    wid = copy.into.missing.fields(dest=wid, source=pre$org.list)
    #wid = copy.into.missing.fields(dest=wid, source=pre)
    len = length(pre[["steps"]])
    if (len>0) {
      wid$show = pre$steps[[len]]$postshow
    }
  }
  wid = copy.into.missing.fields(dest=wid, source=nlist(on.tol))
  wid$data_rows = unlist(wid$data_rows)
  
  # init panes
  pane.names = names(wid$panes)
  
  wid$panes = lapply(pane.names, function(pane.name) {
    pane = wid$panes[[pane.name]]
    pane$name = pane.name
    arg.li = c(pane, wid[setdiff(names(wid),c("panes","click.rel.tol"))])
    get.pane.from.ps(pane = pane.name, ps = ps,arg.li=arg.li, shallow.copy = TRUE)
  })
  names(wid$panes) = pane.names
  
  # init svgs that initially show all curves
  wid$img.ids =  paste0(ps$ps.id,"_panequiz_",bi,"_",pane.names)
  wid$svgs = lapply(seq_along(wid$panes), function(ind) {
    pane.svg(wid$panes[[ind]],id=wid$img.ids[[ind]], show = ".all", display="whisker")  
  })

  show = wid$show
  wid$all.names = unique(unlist(lapply(wid$panes, function(pane) names(pane$objs))))
  
  # compute steps
  given_show = compute.show.list(show=wid$show,hide=wid[["hide"]], data_rows=wid$data_rows, all.names = wid$all.names)

  lines = c(step.lines,length(txt)+1)
  steps = vector("list", length(step.lines))
  names(steps) = str.trim(str.right.of(txt[step.lines], "#. step "))
  i = 1  
  for (i in seq_along(steps)) {
    step = parse.hashdot.yaml(txt[(lines[i]+1):(lines[i+1]-1)])
    step$step.num = i
    step = panequiz.make.step(step, given_show=given_show, panes=wid$panes,ps=ps,bi=bi,wid=wid)
    steps[[i]] = step
    given_show = step$postshow      
  }

  wid$steps = steps
  wid$max.points = sum(sapply(wid$steps, function(step) step$points))
  
  # init layout
  if (is.null(wid$layout)) {
    wid$layout = '
<table style = "table-layout: fixed; width: 100%;"><tr>
<td style="vertical-align: top; width: 40%; padding-top: 0px;">{{menu}}{{text}}</td>
<td style="vertical-align: top;">{{pane}}</td>
</tr></table>
    '    
  }
  
  
  wid$textId = paste0("panequiz_text_",bi)
  wid$paneId = paste0("panequiz_pane_",bi)
  
  textOutput = as.character(uiOutput(wid$textId))
  paneOutput = as.character(uiOutput(wid$paneId)) 
  
  #paneOutput = HTML(wid$svgs[[1]]$html)
  
  menu = as.character(panequiz.menu.bar(bi=bi))
  wid$repl_layout = HTML(replace.whiskers(wid$layout, list(text=textOutput,pane=paneOutput,menu=menu)))
  
  
  return(wid)
}


panequiz.ui.fun = function(ts,bi,...) {
  restore.point("pane.quiz.ui.fun")
  wid = ts$wid
  ui = wid$repl_layout
  panequiz.show.step(ts=ts)
  ui
}

panequiz.show.step = function(step.num=ts$step.num, step=wid$steps[[step.num]], wid=ts$wid, step.mode = ts$step.mode, ts=NULL, msg="", skip.plot=FALSE) {
  restore.point("panequiz.show.step")
  
  # show text
  html = p(step[[paste0(step.mode,".html")]])
  
  if (is.false(wid$direct_click) & step.mode != "success" & step$task.type != "none") {
    btnId = paste0(wid$id,"_checkBtn")
    check.btn = bsButton(btnId,"check",size = "small")
    html = div(html,check.btn)
  }
  
  if (nchar(msg)>0) {
    html = div(html,p(msg))
  }
  setUI(wid$textId,html)
  ts$xy = NULL
  ts$click.pane = NULL

  if (skip.plot) return()
  
  # show pane figure
  pmode = step.mode
  if (step.mode=="failure" | step.mode=="pre") {
    pmode = "pre"
  } else {
    pmode = "post"    
  }

  # need to adapt
  
  pane.ind = 1
  # hide and show genom
  show = step[[paste0(pmode,"show")]]
  
  for (pane.ind in seq_along(wid$panes)) {
    pane = wid$panes[[pane.ind]]
    # pane is first shown
    if (!ts$pane.shown[pane.ind]) {
      html = wid$svgs[[pane.ind]]$html
      
      hide.ids = get.hide.geoms.ids(pane,show)
      li = as.list(rep("none", length(hide.ids)))
      names(li) = paste0("display_",hide.ids)
      
      html = whisker::whisker.render(html, li)
      setUI(wid$paneId,HTML(html))
      ts$pane.shown[pane.ind] = TRUE
    } else {
      svg.id = wid$img.ids[[pane.ind]]
      show.svg.geoms(svg.id = svg.id, pane=pane, show=show)
      hide.svg.geoms(svg.id = svg.id, pane=pane,show=show)
    }
    # hide click ...
    setHtmlAttribute(id=c(pane$circle.marker.id,pane$poly.marker.id),attr=list(display="none"))
  }
}

panequiz.show.next.step = function(ts,...) {
  restore.point("panequiz.show.next.step")
  
  step = ts$wid$steps[[ts$step.num]]
  # first move to success
  if ((ts$step.mode == "pre" | ts$step.mode == "failure") & !isTRUE(step$single.stage)) {
    if (ts$step.num > ts$sts$steps.solved) {
      panequiz.show.step(ts=ts,msg="You must solve the current task, before you can see the next task.")
    }
    ts$step.mode = "success"
    panequiz.show.step(ts=ts)
    return()
  }
    
  next.step.num = ts$step.num +1
  if (next.step.num > length(ts$wid$steps)) {
    rtutor.bubble.click(...)
    return()
  }

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



panequiz.make.step = function(step, panes=wid$panes, given_shows,ps,bi, wid, prev_shows=NULL, prev_src=NULL) {
  restore.point("panequiz.make.step")
  
  no.success = is.null(step$success)
  if (is.null(step$points)) step$points = 0
  if (is.null(step$score)) step$score = 0
  
  pane = panes[[1]]
  
  res = compute.step.shows(step=step, given_show=given_shows, all.names=wid$all.names, data_rows=wid$data_rows)
  step$preshow = res$preshow
  step$postshow = res$postshow

  whisker.values = whisker.values.from.data_rows(data=pane$data, data_rows=pane$data_rows)
  
  pre.html = replace.whiskers(md2html(step$tell), whisker.values)
  success.html = replace.whiskers(md2html(step$success), whisker.values)
  if (is.null(step$failure)) step$failure = "---\nNot yet correct!"
  failure.html = replace.whiskers(md2html(step$failure), whisker.values)
  
  step$pre.html = withMathJax(HTML(pre.html))
  step$success.html = withMathJax(HTML(paste0(pre.html, success.html)))
  step$failure.html = withMathJax(HTML(paste0(pre.html,failure.html)))
  
  step$task.type = "none"
  step = panequiz.make.step.task(step=step, ps=ps,wid=wid) 
  
  
  step$single.stage = no.success & step$task.type == "none"
  
  step
}

panequiz.make.step.task = function(step,panes=wid$panes,ps,wid) {
  restore.point("panequiz.make.step.task")

  data_rows = wid$data_rows
  # NEED TO CORRECT
  pane = panes[[1]]
  
  task.fields = c("find","find_shift")
  step$task.type = task.type = task.fields[task.fields %in% names(step)]
  if (length(step$task.type)==0) step$task.type = "none"
  step$has.task = step$task.type != "none"
  if (!step$has.task) return(step)
  

  task.fun.env = new.env(parent=globalenv())

  # find geom that are specified in the task
  nr = get.geom.name.and.row(step[[task.type]],data_rows, pane=pane)
  if (task.type == "find") {
    restore.point("make.find.task")
    
    task.fun.env$geom = pane$geoms.li[[nr$row]][[nr$name]]
    task.fun.env$on.tol = wid$on.tol
    task.fun = function(xy,pane.name=NULL,...) {
      ok = is.point.on.geom(xy,geom,on.tol = on.tol)
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



panequiz.make.org.task.state = function(wid,...) {
  list(
    solved=FALSE,
    step.num = 1,
    step.mode = "pre",
    points = 0,
    score = 0,
    sts = list(steps.solved = 0),
    wid=wid
  )
}

panequiz.init.task.state = function(ts,ups=NULL, task.ind=ts$task.ind,...) {
  restore.point("panequiz.init.task.state.with.ups")
  
  if (is.null(ups) | is.null(ups$sts[[task.ind]])) {
    if (is.null(ts$sts)) ts$sts = list(steps.solved = 0)
    ts$pane.shown = rep(FALSE,length(ts$wid$panes))
    return(ts)
  }
  ts$step.num = max(1,min(ts$sts$steps.solved, length(ts$wid$steps)))
  ts$step.mode = if (ts$step.num > ts$sts$steps.solved) "pre" else "success"
  
  ts$solved = ups$utt$was.solved[task.ind]
  ts$pane.shown = rep(FALSE,length(ts$wid$panes))
  
  ts
}


panequiz.shown.txt.fun = function(ts,solved=FALSE,...) {
  "\n--A pane quiz--\n"
} 

panequiz.sol.txt.fun = panequiz.out.txt.fun = function(ts,solved=TRUE,...) {
  "\n--A pane quiz--\n"
}

panequiz.init.handlers = function(wid=ts$wid,ps=get.ps(), app=getApp(),ts=NULL,bi,...) {
  restore.point("panequiz.init.handlers")
  
  pane.ind = 1
  for (pane.ind in seq_along(wid$panes)) {
    pane = wid$panes[[pane.ind]]
    img.id = wid$img.ids[[pane.ind]]
    svgClickHandler(id=img.id,fun = panequiz.click,ts=ts,pane=pane) 
  }
 
  
  nextBtnId = paste0("panequizNextBtn_",bi)
  prevBtnId = paste0("panequizPrevBtn_",bi)
  buttonHandler(nextBtnId, panequiz.show.next.step, ts=ts)
  buttonHandler(prevBtnId, panequiz.show.prev.step, ts=ts)
  btnId = paste0(wid$id,"_checkBtn")
  buttonHandler(btnId, panequiz.check.click, ts=ts)
  
  
  
}

panequiz.check.click = function(...,ts=NULL) {
  args = list(...)
  restore.point("panequiz.check.click")
 
  
  
  wid = ts$wid
  step = wid$steps[[ts$step.num]]
  step.mode = ts$step.mode
  
  if (is.null(ts[["xy"]]) | is.null(ts[["click.pane"]])) {
    panequiz.show.step(ts=ts,msg="You have not yet clicked on the figure.")
    return()
  }
  panequiz.check.task(ts=ts)
  
}


panequiz.click = function(x,y,...,ts=NULL,pane=NULL) {
  args = list(...)
  restore.point("panequiz.click")
  cat("panequiz.click")
  
  wid = ts$wid
  step = wid$steps[[ts$step.num]]
  step.mode = ts$step.mode

  if (step.mode == "success" | !step$has.task) {
    panequiz.show.next.step(ts=ts,...)
    return()
  }
  
  # translate pixel to plot coordinates
  ## NEED TO CHANGE FOR MULTIPLE PANES
  coordmap = step$pre.plot[[1]]$coordmap
  dr = wid$svgs[[1]]$svg$dr
  px = x; py = y

  ts$xy = range.to.domain(x=px,y=py, dr=dr)
  ts$click.pane = pane$name  
  
  if (is.false(wid$direct_click)) {
    restore.point("no direct click")
    r = wid$on.tol * min(abs(diff(dr$range$x)),abs(diff(dr$range$y)))

    
    setHtmlAttribute(id=pane$circle.marker.id,list(cx= px, cy= py, r=r, display= "yes"))
    return()
  }
  cat("\npixel = ",c(px,py)," xy = ",unlist(ts$xy))
  
  panequiz.check.task(xy=ts$xy, ts=ts,step=step)
  
}

panequiz.check.task = function(xy=ts$xy, ts=NULL, step = ts$wid$steps[[ts$step.num]]) {
  restore.point("panequiz.check.task")
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


get.geom.name.and.row = function(id,data_rows=1, all.cols = names(pane$objs), pane=NULL) {
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
  #val0 = as.list(data[data_rows[1],])
  val0 = as.list(data[data_rows,,drop=FALSE])
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