
examples.plot.to.html = function() {
  setwd("D:/libraries/EconCurves/")
  addResourcePath("image",getwd())

  filename="test.png"
  res = plot.to.html({
    plot(1:10,(1:10)^2)
    abline(h=5)
    abline(v=5)
    }, format="png", src.path="image", filename = filename, embed=FALSE, compute.coordmap = TRUE, img.id="myimg", img.style="cursor: pointer;")
  html = res$html
  coordmap = res$coordmap
  
  app = eventsApp()
  addResourcePath("fig", getwd())
  app$ui = fluidPage(
    p("Image"),
    HTML(html)
  )
  imageClickHandler(id="myimg", function(...,app=getApp()) {
    args = list(...)
    pixelratio = get.pixelratio()
    restore.point("my.image.handler")
    cat("\nclicked on image pixelratio = ", pixelratio,"\n")
    x = args$x
    y = args$y
    
    cat(paste0(c(x,y)," -> ",scaleInvCoords(x,y,coordmap), collapse="\n"))
  })
  viewApp()

}  
  
plot.to.html = function(expr,envir=parent.frame(), quoted=NULL, width.px=width.in*res, height.px=height.in*res, res=144, width.in=5, height.in=4, pointsize=10, bg="white", format=c("png","svg")[1],out.dir=getwd(),src.path=".", filename=NULL, embed=FALSE, compute.coordmap=FALSE, img.id=NULL, img.style=NULL, img.class=NULL) {
  restore.point("plot.to.html")
  
  library(rmdtools)
 
  if (is.null(quoted)) {
    quoted = substitute(expr)
  }
  
  if (!embed & is.null(filename)) {
    filename = paste0("zzz",random.string(n = 1,nchar=10,set=letters),".",format)
  }
  if (!is.null(img.id)) img.id = paste0(' id = "',img.id,'"')
  if (!is.null(img.class)) img.class = paste0(' class = "',img.class,'"')
  #if (!is.null(img.style)) img.style = paste0(' style = "',img.style,'"')
  
  if (format == "svg") {
    library(svglite)

    if (embed) {
      s <- svgstring(bg=bg, pointsize=pointsize, width=width.in, height=height.in)
      html = s()
      dev.off()
    } else {
      svglite(file = paste0(out.dir,"/",filename),bg=bg, pointsize=pointsize, width=width.in, height=height.in)
    }
    eval(quoted, envir)
    if (compute.coordmap) {
      coordmap = shiny:::getPrevPlotCoordmap(width=width.px, height=height.px)[[1]]
    }
    if (embed) {
      html = s()
      html = paste0('<div style="width: ',width.in,'in; height: ',height.in,'in;">\n',html,'\n</div>')

    } else {
      html = paste0('<img src="',src.path,'/',filename,'" style="width: ',width.in,'in; height: ',height.in,'in;', img.style,';"',img.id,img.class,'>')
    }
    dev.off()
 
  } else if (format == "png") {
    if (embed) {
      restore.point("html.embed.png")
      out.dir = tempdir()
      filename= basename(tempfile(fileext = ".png",tmpdir = out.dir))
      ret = plot.png.with.coordmap(quoted=quoted,width.px = width.px,height.px = height.px, res=res,envir = envir,dir = out.dir,filename = filename)
      library(base64enc)
      enc = base64encode(paste0(out.dir,"/",filename))
      html = paste0('<img src="data:image/png;base64,',enc,'" style="width: ',width.in,'in; height: ',height.in,'in;', img.style,'"', img.class, img.id,'>')
      #html = paste0('<img src="',src.path,'/',filename,'"',img.id,img.class, img.style,'>')
      #html = paste0('<div style="width: ',width.in,'in; height: ',height.in,'in;">\n',html,'\n</div>')
    } else {
      restore.point("html.external.png")
      ret = plot.png.with.coordmap(quoted=quoted,width.px = width.px,height.px = height.px, res=res,envir = envir,dir = out.dir,filename = filename)
      html = paste0('<img src="',src.path,'/',filename,'" style="width: ',width.in,'in; height: ',height.in,'in;', img.style,'"', img.class, img.id,'>')
      #html = paste0('<img src="',src.path,'/',filename,'"',img.id,img.class, img.style,'>')
      #html = paste0('<div style="width: ',width.in,'in; height: ',height.in,'in;">\n',html,'\n</div>')
    }
    coordmap = ret$coordmap
    
  }
  
  ret = list(html=html, filename=filename, out.dir=out.dir)
  if (compute.coordmap) {
    ret$coordmap = coordmap
  }
  return(ret)
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
    tags$img(src = paste0("fig/",filename),id="myimg", style="cursor: crosshair;")
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

plot.png.with.coordmap = function(expr,width.px=width.in*res, height.px=height.in*res, res=144, width.in=4, height.in=3, envir=parent.frame(), quoted=NULL, filename = tempfile(tmpdir = dir,fileext = ".png"), dir=tempdir(), pixelratio=1,...) {
  
  restore.point("plot.png.with.coordmap")
  
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
  filename = paste0(dir,"/",basename(filename))
  pngfun(filename = filename, width = width.px, height = height.px, 
      res = res, ...)
  #op <- graphics::par(mar = rep(0, 4))
  #tryCatch(graphics::plot.new(), finally = graphics::par(op))
  #op <- graphics::par(mar = rep(0, 4))
  tryCatch(graphics::plot.new())
  dv <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dv), add = TRUE)
  
  eval(quoted, envir)
  coordmap = shiny:::getPrevPlotCoordmap(width=width.px, height=height.px)[[1]]
 
  list(filename=filename, dir=dir, coordmap=coordmap)

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