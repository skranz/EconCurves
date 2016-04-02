
examples.plot.to.html = function() {
  plot.to.html(plot(1:10))
}  
  
plot.to.html = function(expr,env=parent.frame(), quoted=NULL, width=4, height=3, pointsize=10, bg="white") {
  library(rmdtools)
  library(svglite)
 
  if (is.null(quoted)) {
    quoted = substitute(expr)
  }
  restore.point("plot.to.html")
  
  s <- svgstring(bg=bg, pointsize=pointsize, width=width, height=height)
  eval(quoted, env)
  html = s()
  dev.off()
  
  paste0('<div style="width: ',width,'in; height: ',height,'in;">\n',html,'\n</div>')

}

get.pixelratio = function(session = app$session, app=getApp()) {
  if (is.null(session)) return(1)
  session$clientData$pixelratio %OR% 1  
}

examples.plot.png.with.coordmap = function() {
  setwd("D:/libraries/EconCurves/")
  filename="test.png"
  res = plot.png.with.coordmap(plot(1:10), width.px = 400, height.px=300, dir=getwd(), filename=filename)
  coordmap=res$coordmap[[1]]
  library(shinyEvents)
  app = eventsApp()
  addResourcePath("fig", getwd())
  app$ui = fluidPage(
    p("Image"),
    tags$img(src = paste0("fig/",filename),id="myimg")
  )
  imageClickHandler(id="myimg", function(...,app=getApp()) {
    args = list(...)
    pixelratio = get.pixelratio()
    restore.point("my.image.handler")
    cat("\nclicked on image pxielratio = ", pixelratio,"\n")
    x = args$x
    y = args$y
    
    cat(paste0(c(x,y)," -> ",scaleInvCoords(x,y,coordmap), collapse="\n"))
  })
  viewApp()
}

plot.png.with.coordmap = function(expr,width.px=width.in*res, height.px=height.in*res, res=72, width.in=4, height.in=3, envir=parent.frame(), quoted=NULL, filename = tempfile(tmpdir = dir,fileext = ".png"), dir=tempdir(), pixelratio=1) {
  
  restore.point("plot.png.with.coordmap")
  
  if (is.null(quoted)) quoted = substitute(expr)  
  width = width.px; height = height.px;
  
  if (capabilities("aqua")) {
      pngfun <- grDevices::png
  }
  else if ((getOption("shiny.usecairo") %OR% TRUE) && nchar(system.file(package = "Cairo"))) {
      pngfun <- Cairo::CairoPNG
  }
  else {
      pngfun <- grDevices::png
  }
  pngfun(filename = filename, width = width, height = height, 
      res = res, ...)
  op <- graphics::par(mar = rep(0, 4))
  tryCatch(graphics::plot.new(), finally = graphics::par(op))
  dv <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dv), add = TRUE)
  
  eval(quoted, envir)
  coordmap = shiny:::getPrevPlotCoordmap(width=width.px, height=height.px)
 
  list(filename=filename, dir=dir, coordmap=coordmap)

}

plot.png = function (expr, filename = tempfile(fileext = ".png"), width = 400, 
    height = 400, res = 72, quoted=NULL, envir=parent.frame(),...) 
{
  restore.point("plot.png")
  
  if (is.null(quoted)) quoted = substitute(expr)
  if (capabilities("aqua")) {
      pngfun <- grDevices::png
  }
  else if ((getOption("shiny.usecairo") %OR% TRUE) && nchar(system.file(package = "Cairo"))) {
      pngfun <- Cairo::CairoPNG
  }
  else {
      pngfun <- grDevices::png
  }
  pngfun(filename = filename, width = width, height = height, 
      res = res, ...)
  op <- graphics::par(mar = rep(0, 4))
  tryCatch(graphics::plot.new(), finally = graphics::par(op))
  dv <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dv), add = TRUE)
  
  eval(quoted, envir)
  filename
}

`%OR%` = function (x, y) 
{
    if (is.null(x) || isTRUE(is.na(x))) 
        y
    else x
}

# Copied from shiny source code
# Scale x and y coordinates from domain to range, using information in
# scaleinfo. scaleinfo must contain items $domain, $range, and $log. The
# scaleinfo object corresponds to one element from the coordmap object generated
# by getPrevPlotCoordmap or getGgplotCoordmap; it is the scaling information for
# one panel in a plot.
scaleCoords <- function(x, y, scaleinfo) {
  if (is.null(scaleinfo))
    return(NULL)

  domain <- scaleinfo$domain
  range <- scaleinfo$range
  log <- scaleinfo$log

  list(
    x = shiny:::scale1D(x, domain$left, domain$right, range$left, range$right, log$x),
    y = shiny:::scale1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
  )
}

# Copied from shiny source code
# Inverse scale x and y coordinates from range to domain, using information in
# scaleinfo.
scaleInvCoords <- function(x, y, scaleinfo) {
  if (is.null(scaleinfo))
    return(NULL)

  domain <- scaleinfo$domain
  range <- scaleinfo$range
  log <- scaleinfo$log

  list(
    x = shiny:::scaleInv1D(x, domain$left, domain$right, range$left, range$right, log$x),
    y = shiny:::scaleInv1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
  )
}