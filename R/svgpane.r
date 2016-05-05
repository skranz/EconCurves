
example.plot.pane = function() {

opc.pane = pane(xvar="u",yvar="dw",xlab="Arbeitslosigkeitsquote (u)", ylab="Relative Lohnsteigerung", xrange=c(0,0.2), yrange=c(-0.1,0.2))
plot.curve(eq = (dw ==-0.1 + 1.1*u), pane=opc.pane, color="blue", main="Hypothese 1")
abline(h=0)
  
yaml = '
pane:
  curves:
    supply:
      label: S{{idS}}
      eq: p == mc
      color: blue
      tooltip: The supply function S.
    demand:
      label: D{{idD}}
      eq: y == A - b *p
      color: red
      tooltip: The demand function D.
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,150]
  xmarkers: [y_eq]
  ymarkers: [p_eq]
'
  pane = init.yaml.pane(yaml=yaml)
  pane$params = list(A=100, b=1, mc=20,y_eq=30, p_eq=40, idD=1,idS="")
  make.pane.data(pane)

  compute.pane.geoms(pane)
  
  html = pane.svg(pane)
  cat(html)
  
  library(rmdtools)
  view.html(text=html)
  
  plot.pane(pane)
  
  res = locator(1)
  
}


#' Plot a pane
pane.svg = function(pane, show = pane$show, hide=pane$hide, xrange=pane$xrange, yrange=pane$yrange, alpha=1,main="",mar=NULL, show.grid=!TRUE, label.df=NULL,lwd.factor=1,label.cex=0.75, cex.axis=0.8, 
  xlab= if (is.null(pane$xlab)) pane$xvar else pane$xlab,
  ylab= if (is.null(pane$ylab)) pane$yvar else pane$ylab,
compute.geoms=TRUE, params = pane$params, data=pane$data, data_rows=1
  ) {
  restore.point("plot.pane")
  
  pane$data = data
  pane$params = params
  pane$yrange = yrange
  pane$xrange = xrange

  missing.cols = check.for.missing.data.cols(pane,pane$data, show=show)

  if (compute.geoms)
    compute.pane.geoms(pane=pane,data_rows=data_rows)
  
  svg = new_svg(width=460, height=320, xlim=xrange, ylim=yrange) %>%
    svg_xaxis() %>%
    svg_yaxis()
  
  geoms = NULL
  i = 1
  if (identical(show,".all")) {
    show = names(pane$objs)
  }  
  for (i in seq_along(data_rows)) {
    row = data_rows[i]
    if (is.list(show)) {
      cur.show = show[[i]]
    } else {
      cur.show = show
    }
    if (is.list(hide)) {
      cur.show = setdiff(cur.show, hide[[i]])
    } else {
      cur.show = setdiff(cur.show, hide)  
    }
    cur.geoms = pane$geoms.li[[row]][cur.show]   

    geoms = c(geoms, cur.geoms)
  }
  
  if (is.null(geoms)) {
    cat("\nNo geoms drawn...")
    return()
  }

  if (is.null(label.df))
    label.df = find.label.pos(geoms,yrange=pane$yrange)

  #boxed.labels(x = label.df$x,y = label.df$y,labels = label.df$label,cex=label.cex,bg="white",border=FALSE,xpad=1.1,ypad=1.1)

    
  for (geom in geoms) {
    draw.svg.geom(svg,geom)
  }

  pane$geoms = geoms
  pane$svg = svg
  invisible(svg_string(svg))
}

draw.svg.geom = function(svg,geom) {
  if (geom$type=="curve") {
    draw.svg.curve(svg,geom)
  } else if (geom$type=="marker") {
    draw.svg.marker(svg,geom)
  }
  svg
}

draw.svg.curve = function(svg,geom, level=10) {
  svg_polyline(svg, x=geom$x,y=geom$y, stroke=geom$color, level=level, tooltip=geom$obj$tooltip)
}
draw.svg.marker = function(svg, geom, level=2) {
  svg_polyline(svg, x=geom$x,y=geom$y, stroke=geom$color, stroke_width=0.5, level=level,tooltip=geom$obj$tooltip)
}