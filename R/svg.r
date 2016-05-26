
examples.svg = function() {
  library(dplyr)
  library(rmdtools)
  library(svglite)
  library(EconCurves)

  code = svg_from_plot(plot(1:10))$code
  cat(code)

  xrange = c(-10,10)
  yrange = c(0,100)

  svg = new_svg(xlim=xrange,ylim=yrange) %>%
    svg_xaxis(label="The x-axis") %>%
    svg_yaxis(label="The y-axis") %>%
    svg_boxed_label(x=-8,y=50,text="A label") %>%
    svg_polyline(x=(-10):10,y=((-10):10)^2,stroke = "blue", tooltip="I am a <bold>parabel</bold>.")
    

  html = svg_string(svg)
  cat(html)
  #html = paste0(html, collapse="\n")
  view.html(text=sep.lines(html))
}



svg_from_plot = function(call, width=500, height=400, envir=parent.frame(), bg="white",pointsize=1, pixel.per.inch=72, level=0, id=NULL) {
  quoted = substitute(call)
  restore.point("svg_from_plot")


  s <- svgstring(bg=bg, pointsize=pointsize, width=width / pixel.per.inch, height=height / pixel.per.inch, standalone = TRUE)

  eval(quoted, envir)
  code = sep.lines(s())
  head = code[1:2]
  code = code[-c(1:2,length(code))]
  dev.off()

  svg.code = svglite:::inlineSVG(eval(quoted,envir))

  el =id_char(id=id,code, collapse="\n")
  el.level = level
  names(el.level) = id

  coordmap = shiny:::getPrevPlotCoordmap(width=width, height=height)[[1]]

  svg = new.env()
  svg$head = head
  svg$width = width
  svg$height = height
  svg$coordmap = coordmap
  svg$dr =list(
    range=list(x=unlist(coordmap$range[1:2]), y=unlist(coordmap$range[3:4])),
    domain = list(x=unlist(coordmap$domain[1:2]), y=unlist(coordmap$domain[3:4]))
  )
  #svg$dr = make.domain.range(xlim=xlim,ylim=ylim,width=width, height=height)
  svg$el = el
  svg$el.level = el.level
  svg$defs
  svg$code = svg.code
  svg = svg_def_label_box(svg)
  svg
}


new_svg = function(width=500, height=400, xlim=c(0,1),ylim=xlim,id=NULL, css=default_svg_css(), margins=c(bottom=80,left=100, top=40, right=50), class="clickable_svg") {
  restore.point("svg")

  if (is.null(id))
    id = paste0("svg_",random.string(1))
  svg = new.env()
  
  svg$id = id
  svg$head = paste0("<svg width='",width,"' height='",height,"' id = '",id,"' class='",class,"'>")
  svg$width = 500
  svg$height = height
  svg$dr = make.domain.range(xlim=xlim,ylim=ylim,width=width, height=height, margins=margins)
  svg$el = NULL
  svg$el.level = NULL
  svg$defs = NULL
  svg_add_def(svg, id="default_css", defs = paste0('
  <style type="text/css"><![CDATA[',
    css,'
  ]]>
  </style>
  '))
  svg_def_label_box(svg)
  
  svg
}

default_svg_css = function() {
'
line, polyline, path, rect, circle {
  fill: none;
  stroke: #000000;
  stroke-linecap: round;
  stroke-linejoin: round;
  stroke-miterlimit: 10.00;
}

.polyline {
  stroke-width: 2;
  stroke-opacity: 0.8;
}

.polyline:hover {
  stroke-width: 5;
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
  font-size: 10.00pt;
  font-family: Arial;
  font-weight: normal;
  filter: url(#label_box);
}


.axis-label {
  font-size: 11.00pt;
  font-family: Arial;
  font-weight: normal
}


'
}

svg_add = function(svg, el, id = names(el), level=0) {
  if (is.null(id)) {
    svg$el = c(svg$el,el)
    svg$el.level = c(svg$el.level, rep(level, length.out=length(el)))
  } else {
    svg$el[id] = el
    svg$el.level[id] = level
  }
  svg
}


svg_add_def = function(svg, defs, id = names(defs)) {
  if (is.null(id)) {
    svg$defs = c(svg$defs,defs)
  } else {
    svg$defs[id] = defs
  }
  svg
}


svg_string = function(svg) {
  defs = if (length(svg$defs)>0) c('<defs>', svg$defs,'</defs>') else NULL
  paste0(
    c(
      svg$head,
      defs,
      svg$el[order(svg$el.level)],
      "</svg>"
    ),
    collapse = "\n"
  )
}

make.domain.range = function(xlim,ylim,width=500, height=400, margins=c(bottom, left, top, right), bottom=height / 10, top = height/10, left = width/8, right=width/20) {
  restore.point("make.domain.range")
  
  list(
    domain=list(x=xlim,y=ylim),
    range = list(
      x=c(margins[2],width-margins[4]),
      y=rev(c(margins[3],height-margins[1]))
    )
  )
}

domain.to.range = function(x=NULL,y=NULL,domain=dr$domain, range=dr$range,dr=svg$dr, svg=NULL, to.range=TRUE) {
  restore.point("domain.to.range")
  if (!to.range) {
    if (is.null(x)) return(y)
    if (is.null(y)) return(x)
    return(list(x=x,y=y))
  }
  
  if (!is.null(x))
    x = ((x - domain$x[1]) /(domain$x[2]-domain$x[1])) * (range$x[2]-range$x[1]) + range$x[1]
  if (!is.null(y))
    y = ((y - domain$y[1]) /(domain$y[2]-domain$y[1])) * (range$y[2]-range$y[1]) + range$y[1]


  if (is.null(y)) return(x)
  if (is.null(x)) return(y)
  nlist(x,y)
}


range.to.domain = function(x=NULL,y=NULL,domain=dr$domain, range=dr$range,dr=svg$dr, svg=NULL) {
  restore.point("range.to.domain")

  if (!is.null(x))
    x = ((x - range$x[1]) /(range$x[2]-range$x[1])) * (domain$x[2]-domain$x[1]) + domain$x[1]
  if (!is.null(y))
    y = ((y - range$y[1]) /(range$y[2]-range$y[1])) * (domain$y[2]-domain$y[1]) + domain$y[1]


  if (is.null(y)) return(x)
  if (is.null(x)) return(y)
  nlist(x,y)
}


make_style_arg = function(style) {
  if (is.list(style)) {
    style = style[!sapply(style,is.null)]
    if (length(style)==0) return(NULL)
    na = names(style)
    na = gsub("_","-",na, fixed=TRUE)
    style = paste0(na,": ",style, collapse="; ")
  }
  style
}

svg_tag = function(name, args, inner=NULL, .quote='"',tooltip=NULL) {
  args = args[!sapply(args, is.null)]
  args$style = make_style_arg(args$style)
  arg.str = paste0(names(args),"=",.quote,args,.quote, collapse=" ")

  if (!is.null(tooltip)) {
    inner =c(paste0("<title>",tooltip,"</title>"),inner)
  }
  if (is.null(inner)) {
    str = paste0("<",name," ", arg.str,"/>")
  } else {
    str = paste0("<",name," ", arg.str,">\n",paste0(inner,collapse="\n"),"</",name,">")
  }
  str
}

html_arg_str = function(..., .quote='"') {
  args = list(...)
  restore.point("html_arg_str")

  args$style = make_style_arg(args$style)
  args = args[!sapply(args, is.null)]
  arg.str = paste0(names(args),"=",.quote,args,.quote, collapse=" ")
  arg.str
}


svg_polyline = function(svg, x,y,id=NULL, class="polyline",style=c(nlist(fill, stroke,stroke_width), extra.style), fill="none", stroke="black",stroke_width=NULL, extra.style=list(), level=0, tooltip=NULL, extra.args = list(...),...) {
  restore.point("svg_polyline")
  rp = domain.to.range(x=x,y=y,svg=svg)
  points = paste0(rp$x,",",rp$y, collapse=" ")
  el = svg_tag("polyline", c(nlist(points,id,class,style), extra.args), tooltip=tooltip)
  svg_add(svg,el,id,level=level)
}

svg_boxed_label = function(svg, x,y, text,id=NULL, class="boxed-label",style=c(nlist("font-size"=font_size), extra.style), font_size=NULL, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE,...) {
  restore.point("svg_boxed_label")
  rp = domain.to.range(x=x,y=y,svg=svg, to.range=to.range)
  el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,class,style,...), tooltip=tooltip, inner=text)
  svg_add(svg,el,id,level=level)
}


svg_axis = function(svg,...) {
  restore.point("svg_axis")
  svg_xaxis(svg,...)
  svg_yaxis(svg,...)
}

svg_xaxis = function(svg, id="xaxis", label=NULL, latex = NULL,  align="default", dr=svg$dr, return.string=FALSE, level=100, num.ticks=5, ticks =pretty.ticks(dr$domain$x, n=num.ticks), tick.size = 10, arrow=!show.ticks, show.ticks = TRUE, show.tick.labels=show.ticks, class.group= "axis x-axis",  class.line="axis-main", class.tick="axis-tick",class.tick.label="axis-ticklabel", class.label="axis-label", style.line=NULL, style.tick=NULL,style.tick.label=NULL, style.label=NULL, axis.offset=if (show.ticks) 10 else 0, axis.label.offset=if (show.ticks) 30 else 8,...) {
  restore.point("svg_xaxis")
  x.ax = dr$range$x
  if (align=="default" || align == "bottom") {
    align = dr$range$y[1] + axis.offset
  } else if (align=="top") {
    align = dr$range$y[2] - axis.offset
  } else if (align=="zero") {
    align = max(dr$range$y[1],domain.to.range(y=align,svg = svg))
  } else if (is.numeric(align)) {
    align = domain.to.range(y=align,svg = svg)
  }
  y.ax = rep(align,2)

  if (arrow) {
    svg_def_arrow_head(svg)
    arrow.li = list("marker-end"="url(#arrow_head)")
  } else {
    arrow.li = NULL
  }
   line = svg_tag("line", c(nlist(x1=x.ax[1],x2=x.ax[2]+10,y1=y.ax[1],y2=y.ax[2], style=style.line, class=class.line), arrow.li))

  y1.tick = y.ax[1]
  y2.tick = y1.tick + tick.size * show.ticks
  x.ticks = domain.to.range(x=ticks,svg = svg)

  if (show.ticks) {
    ti.str = paste0('<line x1="',x.ticks,'" x2="',x.ticks,'" y1="',y1.tick,'" y2="',y2.tick,'" ', html_arg_str(style=style.tick, class=class.tick),'/>')
  } else {
    ti.str = ""
  }
  
  if (show.tick.labels) {
    ti.lab = paste0('<text x="',x.ticks,'" y="',y2.tick+15,'" ', html_arg_str(style=style.tick.label, class=class.tick.label),'text-anchor="middle">',ticks,"</text>")
  } else {
    ti.lab = ""
  }

  if (!is.null(latex)) {
    label = latex.to.textspan(latex)
  }
  if (!is.null(label)) {
    y.lab =  y2.tick+axis.label.offset
    x.lab = x.ax[2]+arrow*10
    label = svg_tag(name = "text",args=list(x=x.lab,y=y.lab,style=style.label,class=class.label, "text-anchor"="right"),inner=label)
  }
  
  inner = c(line, ti.str, ti.lab,label)

  g = svg_tag("g", nlist(id,class=class.group),inner=inner)
  svg_add(svg,g,id=id, level=level)
}


svg_yaxis = function(svg, id="yaxis", label=NULL,latex = NULL,
  align="default", dr=svg$dr, return.string=FALSE, level=100, num.ticks=5, ticks =pretty.ticks(dr$domain$y, n=num.ticks), tick.size = 10, arrow=!show.ticks, show.ticks = TRUE, show.tick.labels=show.ticks,
  axis.offset = if (show.ticks) 10 else 0, axis.label.offset=20,
  class.group= "axis y-axis",  class.line="axis-main", class.tick="axis-tick",class.tick.label="axis-ticklabel", class.label="axis-label",
  style.line=NULL, style.tick=NULL,style.tick.label=NULL, style.label=NULL,...  ) {
  restore.point("svg_yaxis")

  y.ax = dr$range$y
  if (align == "default" || align=="left") {
    align = dr$range$x[1] - axis.offset
  } else if (align=="zero") {
    align = max(dr$range$x[1],domain.to.range(x=align,svg = svg))
  } else if (align=="right") {
    align = dr$range$y[2] + axis.offset
  } else if (is.numeric(align)) {
    align = domain.to.range(x=align,svg = svg)
  }
  x.ax = rep(align,2)

  if (arrow) {
    svg_def_arrow_head(svg)
    arrow.li = list("marker-end"="url(#arrow_head)")
  } else {
    arrow.li = NULL
  }

  svg_def_arrow_head(svg)
  line = svg_tag("line", c(nlist(x1=x.ax[1],x2=x.ax[2],y1=y.ax[1],y2=y.ax[2], style=style.line, class=class.line), arrow.li))

  x1.tick = x.ax[1] - tick.size * show.ticks
  x2.tick = x1.tick + tick.size * show.ticks
  y.ticks = domain.to.range(y=ticks,svg = svg)
  if (show.ticks) {
    ti.str = paste0('<line x1="',x1.tick,'" x2="',x2.tick,'" y1="',y.ticks,'" y2="',y.ticks,'" ', html_arg_str(style=style.tick, class=class.tick),'/>')
  } else {
    ti.str = ""
  }

  if (show.tick.labels) {
    ti.lab = paste0('<text x="',x1.tick-3,'" y="',y.ticks,'" ', html_arg_str(style=style.tick.label, class=class.tick.label),' text-anchor="end" alignment-baseline="middle">',ticks,"</text>")
  } else {
    ti.lab = ""
  }

  if (!is.null(latex)) {
    label = latex.to.textspan(latex)
  }
  if (!is.null(label)) {
    x.lab =  x1.tick
    y.lab = y.ax[2]-10-arrow*10
    label = svg_tag(name = "text",args=list(x=x.lab,y=y.lab,style=style.label,class=class.label,  "text-anchor"="middle"),inner=label)
  }
  
  inner = c(line, ti.str, ti.lab,label)

  g = svg_tag("g", nlist(id,class=class.group),inner=inner)
  svg_add(svg,g,id=id, level=level)
}



pretty.ticks = function(x,n=5,max.rel.out = 0.01,...) {
  ticks = pretty(x,n,...)
  # remove ticks that are too far outside the range
  tol = (max(x)-min(x))*max.rel.out
  ticks = ticks[ticks >= min(x)-tol & ticks<=max(x)+tol]
  ticks
}
svg_def_arrow_head =  function(svg,id="arrow_head", class="arrow_head") {
  svg_add_def(svg=svg,id=id,
    paste0('
    <marker id="',id,'" class="',class,'" markerWidth="10" markerHeight="10" refx="0" refy="3" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L0,6 L9,3 z" style ="fill: black;"/>
    </marker>
    '
    )
  )
}

svg_def_label_box =  function(svg,id="label_box", class="label_box") {
  svg_add_def(svg=svg,id=id,
    paste0(
    '
      <filter x="0" y="0" width="1" height="1" id="', id,'">
        <feFlood flood-color="white" flood-opacity="0.85"/>
        <feComposite in="SourceGraphic"/>
      </filter>
    '
    )
  )
#<text filter="url(#solid)" x="20" y="50" font-size="50">solid background
}       
        
id_char = function(id,...) {
  res = paste0(...)
  names(res) = id
  res
}
