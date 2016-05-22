
example.plot.pane = function() {

opc.pane = pane(xvar="u",yvar="dw",xlab="Arbeitslosigkeitsquote (u)", ylab="Relative Lohnsteigerung", xrange=c(0,0.2), yrange=c(-0.1,0.2))
plot.curve(eq = (dw ==-0.1 + 1.1*u), pane=opc.pane, color="blue", main="Hypothese 1")
abline(h=0)
  
yaml = '
pane:
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,150]
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
  xmarkers:
    y_eq:
      tooltip: The equilibrium output
      color: green
      lineto: demand
  ymarkers:
    p_eq:
  xlab: y
  ylab: p
'
  pane = init.yaml.pane(yaml=yaml)
  pane$params = list(A=100, b=1, mc=20,y_eq=30, p_eq=40, idD=1,idS="")
  make.pane.data(pane)

  compute.pane.geoms(pane)
  
  html = pane.svg(pane)$html
  cat(html)
  
  library(rmdtools)
  view.html(text=html)
  
  plot.pane(pane)
  
  res = locator(1)
  
}


#' Plot a pane
pane.svg = function(pane, id=NULL, show = pane$show, hide=pane$hide, xrange=pane$xrange, yrange=pane$yrange, show.grid=!TRUE,  xlab= if (is.null(pane$xlab)) pane$xvar else pane$xlab,
  ylab= if (is.null(pane$ylab)) pane$yvar else pane$ylab,
compute.geoms=TRUE, params = pane$params, data=pane$data, data_rows=first.non.null(pane$data_rows,1), roles=NULL, css=default_svgpane_css(), show_ticks = first.non.null(pane$show_ticks,TRUE), show_tick_labels=first.non.null(pane$show_tick_labels,show_ticks, TRUE),width=first.non.null(pane$width,pane$org.width,480), height=first.non.null(pane$height,pane$org.height,320), margins=pane$margins,...
  ) {
  restore.point("pane.svg")

  if (is.null(roles)) {
    roles = lapply(seq_along(data_rows), function(ind) {
      default.role(ind, data_rows[[ind]])
    })
  }
  
  pane$data = data
  pane$params = params
  pane$yrange = yrange
  pane$xrange = xrange
  pane$show_ticks = show_ticks
  pane$show_tick_labels = show_tick_labels
  pane$height = height
  pane$width = width
  
  
  missing.cols = check.for.missing.data.cols(pane,pane$data, show=show)

  if (compute.geoms) {
    compute.pane.geoms(pane=pane,data_rows=data_rows)
    xrange = pane$xrange
    yrange = pane$yrange
  }
  svg = new_svg(id=id,width=width, height=height, xlim=pane$xrange, ylim=pane$yrange,css=css, margins=margins,...) %>%
    svg_xaxis(label=pane$xlab, show.ticks = pane$show_ticks, show.tick.labels = pane$show_tick_labels) %>%
    svg_yaxis(label=pane$ylab, show.ticks = pane$show_ticks, show.tick.labels = pane$show_tick_labels)
  
  geoms = NULL
  i = 1
  if (identical(show,".all")) {
    show = names(pane$objs)
  }
  role.inds = NULL
  label.postfix = NULL
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
    label.postfix = c(label.postfix,rep(row,length(cur.geoms)))
    geoms = c(geoms, cur.geoms)
    role.inds = c(role.inds, rep(i,length(cur.geoms)))
  }
  
  if (is.null(geoms)) {
    cat("\nNo geoms drawn...")
    return()
  }

  for (i in seq_along(geoms)) {
    draw.svg.geom(svg,geoms[[i]], role=roles[[role.inds[i]]])
  }

  place.svg.pane.labels(svg,geoms=geoms, pane=pane, label.postfix=label.postfix)

  if (!is.null(pane$title)) {
    x.tit = domain.to.range(x=mean(pane$xrange),svg=svg)
    el = svg_tag("text", nlist(x=x.tit,y=15,class="boxed-label pane-title","text-anchor"="middle"), inner=pane$title)
    
    svg_add(svg,el,level=90)
  }
    
  pane$geoms = geoms
  pane$svg = svg
  invisible(list(svg=svg,html=svg_string(svg)))
}

place.svg.pane.labels = function(svg, geoms, pane, left.offset=5, bottom.offset=10, label.df = NULL, label.postfix) {
  restore.point("place.svg.pane.labels")
  
  labels = sapply(seq_along(geoms), function(i) {
    geom = geoms[[i]]
    geom.label(geom=geom,label.replace = c(geom$values, list(".id"=label.postfix[[i]])))
  })
  if (is.null(label.df))
    label.df = find.label.pos(geoms,xrange=pane$xrange, yrange=pane$yrange)
  
  rp = domain.to.range(x=label.df$x,y=label.df$y, svg=svg)
  x.min = domain.to.range(x=pane$xrange[1],svg=svg) + left.offset
  y.min = domain.to.range(y=pane$yrange[1],svg=svg) - bottom.offset
  is.right = label.df$x == pane$xrange[[2]]
  is.left = label.df$x == pane$xrange[[1]]
  
  
  #rp$x = pmax(rp$x,x.min)
  #rp$y = pmin(rp$y,y.min)
  
  rp$x[is.right] = pane$width-3
  #label.xy = domain.to.range(x=label.df$x,y=label.df$y)
  
  anchor = ifelse(is.right,"end",ifelse(is.left,"start", "middle"))
  
  for (r in seq_len(NROW(label.df))) {
    ind = label.df$ind[r]
    svg_boxed_label(svg,x=rp$x[r],y=rp$y[r], text=labels[ind],id=paste0("geomlabel_",geoms[[ind]]$name,"_",ind), level=100,to.range = FALSE, "text-anchor"=anchor[r], tooltip = geoms[[ind]]$tooltip)
  }

  
}

draw.svg.geom = function(svg,geom, role,...) {
  restore.point("draw.svg.geom")
  
  geom$color = geom.color(geom=geom, role=role)
  geom$tooltip = replace.latex.with.unicode(replace.whiskers(geom$obj$tooltip, geom$values))
  if (geom$type=="curve") {
    draw.svg.curve(svg,geom, role=role)
  } else if (geom$type=="marker") {
    draw.svg.marker(svg,geom, role=role)
  }
  svg
}

draw.svg.curve = function(svg,geom, role=NULL, level=10) {
  restore.point("draw.svg.curve")
  
  svg_polyline(svg, x=geom$x,y=geom$y, stroke=geom$color, level=level, tooltip=geom$tooltip,class = "curve")
}
draw.svg.marker = function(svg, geom, role=NULL, level=2) {
  restore.point("draw.svg.marker")
  
  svg_polyline(svg, x=geom$x,y=geom$y, stroke=geom$color, level=level,tooltip=geom$tooltip, extra.args = list("stroke-dasharray"=geom$obj$dashed), class="marker_line")
}

default.role = function(ind,row) {
  list(
    color.level = min(ind,3),
    role = row
  )
}

default_svgpane_css = function() {
'
line, polyline, path, rect, circle {
  fill: none;
  stroke: #000000;
/*
  stroke-linecap: round;
*/
  stroke-linejoin: round;
  stroke-miterlimit: 10.00;
}

.curve {
  stroke-width: 3;
  stroke-opacity: 0.8;
}

.curve:hover {
  stroke-width: 5;
}

.marker_line {
  stroke-width: 2;
  stroke-opacity: 0.8;
  stroke-dasharray: "5,5";
  stroke-color: "#333333";
}

.marker_line:hover {
  stroke-width: 4;
}


.axis {

}

.axis-main {
  stroke-width: 1.5;
}

.axis-tick {
  stroke-width: 0.5;
}

.axis-ticklabel {
  font-size: 10.00pt;
  font-family: Arial;
  font-weight: normal;
}


.boxed-label {
  font-size: 11.00pt;
  font-family: Arial;
  font-weight: normal;
  filter: url(#label_box);
}


.label_subscript {
  font-size: 8.00pt;
}


.axis-label {
  font-size: 11.00pt;
  font-family: Arial;
  font-weight: normal
}


'
}
