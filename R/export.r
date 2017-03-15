fetch.ps = function(ps = NULL, rps.file=NULL, rmd.file=NULL) {
  library(RTutor2)
  if (!is.null(rmd.file)) {
    txt = readLines(rmd.file)
  }
  if (!is.null(txt)) {
    ps = rtutor.make.frame.ps(txt=txt)
  } else if (!is.null(rps.file)) {
    ps = read.ps(rps.file)
  }
  
}

make.ps.html = function() {
  setwd("D:/lehre/makro")
  outdir = paste0(getwd(),"/figures")
  rmd.file = "makro101.rmd"
  ps = fetch.ps(rmd.file = rmd.file)  
  
}

export.ps.svg = function(ps = NULL, rps.file=NULL, rmd.file=NULL, txt=NULL, outdir=paste0(getwd(),"/figures")) {
  restore.point("export.ps.svg")

  setwd("D:/lehre/makro")
  outdir = paste0(getwd(),"/figures")
  rmd.file = "makro101.rmd"
    
  library(RTutor2)
  if (!is.null(rmd.file)) {
    txt = readLines(rmd.file)
  }
  if (!is.null(txt)) {
    ps = rtutor.make.frame.ps(txt=txt)
  } else if (!is.null(rps.file)) {
    ps = read.ps(rps.file)
  }
  

  bdf = ps$bdf
  bis = which(bdf$type == "plotpane")
  for (bi in bis) {
    name = paste0("plotpane_",bdf$name[[bi]],"_",bi)
    export.plotpane(bdf$obj[[bi]]$wid,name=name,outdir=outdir)
  }
}

export.plotpane = function(wid,name=wid$img.id, outdir,format=c("png","svg","pdf"), zoom=1L) {
  restore.point("export.plotpane")
  width = wid$svg$width*zoom
  height = wid$svg$height*zoom
  export.svg(html=wid$html, dest.file=paste0(outdir,"/",name),format=format, width=width, height=height)
}