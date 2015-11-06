
example.draw.clicks = function() {

yaml = '
pane:
  curves:
    demand:
      label: D
      eq: y == A - b *p
      color: red
    supply:
      label: S
      eq: p == mc
      color: blue
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,100]
  xmarkers: [y_eq]
  ymarkers: [p_eq]
'
  pane = init.yaml.pane(yaml=yaml)
  values = list(A=100, b=1, mc=20,y_eq=30, p_eq=40)
  pane$geoms = compute.pane.geoms(pane, values=values)

  plot.pane(pane)
  
  clicks = list()
  
  while(length(clicks)<5) {
    click = locator(1)
    clicks[[length(clicks)+1]] = unlist(click)
    draw.clicks(clicks)
  }
}


draw.click = function(click,x=click[["x"]],y=click[["y"]],pch="+", color=grey(0.2), cex=1.2) {
  points(x,y,pch=pch,col=color,cex=cex)
  
}

draw.clicks = function(clicks,pch="+", color=grey(0.2), cex=1.2, add.line=TRUE) {
  restore.point("draw.clicks")
  
  if ((!is.data.frame(clicks)) & is.list(clicks)) {
    clicks = do.call("rbind",clicks)   
  }
  points(clicks[,1],clicks[,2],pch=pch,col=color,cex=cex)
  lines(clicks[,1],clicks[,2], col=color)
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



check.click.answer = function(es,xy,pane.name,t=es$cur$t, step.num=es$cur$step.num,task=NULL,...) {
  restore.point("check.click.answer")
  em = es$em
  if (is.null(task)) {
    frame = get.dynry.frame(es,t, step.num)
    task = frame$task
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
