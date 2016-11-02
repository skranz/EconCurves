
armd.block.types.df = function(...) {
  restore.point("armd.block.types.df")

  types = c(
    "pane","plotpane","panequiz"
  )
  n = length(types)
  bt.df = data_frame(type=types, package="EconCurves", is.widget= (types=="panequiz"), parse.inner.blocks = FALSE, remove.inner.blocks=TRUE, is.parent=FALSE, is.container = FALSE)

  bt.df
}

armd.parse.pane = function(bi, am, inner.txt = get.bi.inner.txt(bi=bi, am=am), args = get.bi.args(bi=bi, am=am), name=args$name, ...) {
  restore.point("armd.parse.pane")
  pane = init.yaml.pane(yaml=merge.lines(inner.txt), direct=TRUE, name=name)
  make.pane.data(pane=pane, dataenv=am$pre.env)
  am$bdf$name[[bi]] = name
  # use of ao is for backward compatibility with RTutor2
  # we can redesign this later
  am$bdf$obj[[bi]] = list(ao=pane)
  
  
}

armd.parse.plotpane = function(bi, am, type="pane",inner.txt = get.bi.inner.txt(bi=bi, am=am), args = get.bi.args(bi=bi, am=am), name=args$name, ps=am, ...) {
  restore.point("armd.parse.plotpane")
  yaml = merge.lines(inner.txt)
  arg.li = read.yaml(text=yaml)
  if (is.null(arg.li$pane)) arg.li$pane = name
  if (is.null(arg.li[["pane"]])) {
    stop("You have not specified a pane name in your plotpane block.")
  }
  pane = get.pane.from.ps(pane = arg.li$pane, ps = ps,arg.li=arg.li, shallow.copy = TRUE)
  img.id = paste0(am$am.id,"_plotpane_",bi)
  res = pane.svg(pane,id=img.id)

  am$bdf$obj[[bi]] = c(list(img.id=img.id),res)
  am$bdf$ui[[bi]] = tagList(
    HTML(res$html)
  )

}


rtutor.widget.panequiz = function() {
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

