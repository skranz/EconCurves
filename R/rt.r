
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
      
      if (is.character(arg.li$pane)) {
        row = which(bdf$stype=="pane" & bdf$name==args$name)[1]
        if (is.na(row)) {
          stop(paste0("You want to plot the pane ", name, " but have not defined that pane before"))
        }
        pane = bdf$obj[[row]]$ao
      } else {
        pane = init.yaml.pane(pane=arg.li$pane)        
      }
      if (is.null(pane$width.in)) pane$width.in = 5
      if (is.null(pane$height.in)) pane$height.in = 4
      
      
      cols = setdiff(names(arg.li),c("name","pane","height.in","width.in","params"))
      pane[cols] = arg.li[cols]
      if (is.null(pane$params)) pane$params = list()
      pane$params[names(arg.li$params)] = arg.li$params
      
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
