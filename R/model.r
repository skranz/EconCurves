
examples.model = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("ThreeEq")
  em = load.model("ThreeEqFixedM")
  em = load.model("IS_LM_PC")
  em = load.model("AdaptivePricesRandom")
  em = load.model("GreenParadox")
  em = load.model("HongStein")
  em = load.model("General3Eq")
    
  init.model(em)
  init.model.scen(em)
  sim = simulate.model(em)

  c(sd(sim$p),sd(sim$p_adapt),sd(sim$p_fund))
  
  #library(moments)
  #c(kurtosis(sim$p),kurtosis(sim$p_adapt),kurtosis(sim$p_fund))
  
  sim = em$sim
  dyplot.timelines(em$sim,cols = c(em$var.names, names(em$randomVars)),em = em)
  
  acf(diff(sim$p_fund))
  acf(diff(sim$p))
  
  dyplot.timelines(em$sim,cols = c("y_","pi_","r_"),em = em)
}


load.model = function(modelId, file=paste0(modelId,".yaml"), dir=get.ec()$models.path, ec = get.ec()) {
  restore.point("load.model")
  
  tt = load.struct(name="model",file = paste0(dir,"/",file),typeName = "model")
  em = as.environment(tt.object(tt,1))
  em$yaml = attr(tt,"yaml")
  Encoding(em$yaml) <- "UTF-8"

  em
}





#' transform shocks list into a data frame with one row
#' for each (t, shock, par) combination
make.shocks.table = function(shocks) {
  restore.point("make.shocks.table")
  
  name = names(shocks)[1]
  li = lapply(names(shocks), function(name) {
    shock = shocks[[name]]
    
    t = shock$start:(shock$start+shock$duration-1)
    
    var = names(shock$effects)[1]
    li = lapply(names(shock$effects), function(par) {
      formula_ = parse.as.call(shock$effects[[par]])
      formula.li = replicate(formula_, n = length(t))
      data.table(shock = name,t=t,par = par, formula_ = formula.li)
    })
    dt = rbindlist(li)
    dt
  })
  dt =  rbindlist(li) 
  as_data_frame(dt)  
}

init.model = function(em, skip.if.initialized=TRUE,...) {
  restore.point("init.model")
  if (skip.if.initialized & is.true(em$initialized))
    return(invisible())
  init.model.params(em)
  init.model.curves(em)
  #em$curves[[1]]$name
  init.model.panes(em)
  init.model.vars(em)
  
  em$initialized = TRUE
  invisible(em)
}


init.model.panes = function(em) {
  em$panes = lapply(em$panes, function(pane) {
    pane$name = attr(pane,"name")
    pane$xvar = pane$xy[1]
    pane$yvar = pane$xy[2]
    
    xnames = pane$xmarkers; ynames = pane$ymarkers
    pane$xmarkers = lapply(pane$xmarkers, function(marker) {
      marker = list(name=marker)
      marker$axis = "x"
      marker
    })
    names(pane$xmarkers) = xnames
    pane$ymarkers = lapply(pane$ymarkers, function(marker) {
      marker = list(name=marker)
      marker$axis = "y"
      marker
    })
    names(pane$ymarkers) = ynames

    pane$markers = c(pane$xmarkers,pane$ymarkers)
    pane
  })
  invisible(em$panes)
}


init.model.shocks = function(em, shocks=em$shocks) {
  em$shocks = lapply(shocks, function(shock) {
    shock$name = attr(shock,"name")
    shock
  })
  invisible(em$shocks)
}

init.model.params = function(em) {
  em$init.par = lapply(em$params, function(param) {
    param = param$formula
    if (is.null(param)) return(param)
    attributes(param)=NULL
    if (is.character(param)) param = parse.as.call(param)
    param
  }) 
  
}

init.model.scen = function(em,scen.name = names(em$scenarios)[1], scen = em$scenarios[[scen.name]], skip.cluster.init=FALSE) {
  restore.point("init.model.scen")
  
  scen$T = as.numeric(scen$T)
  em$T = scen$T
  
  # init axis ranges
  em$panes = lapply(em$panes, function(pane) {
    pane$xrange = scen$axis[[pane$xvar]]
    pane$yrange = scen$axis[[pane$yvar]]
    pane
  })
  
  em$scen = scen
  em$shocks = scen$shocks
  init.model.shocks(em)
  
  if (!skip.cluster.init) {
    make.init.cluster.df(em=em)
    create.sim.fun(em)
  }
}

init.model.curves = function(em) {
  restore.point("init.model.curves")
  curve = em$curves[[1]]
  em$curves = lapply(em$curves, function(curve) {
    restore.point("kdfgnjdgkj")
    if (is.null(curve$name))
      curve$name = get.name(curve)
    curve$eq_ = parse.as.call(text=curve$eq)
    curve$impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(curve$eq_),rhs=get.rhs(curve$eq_)))
    
    curve$xvar = curve$xy[1]
    curve$yvar = curve$xy[2]
    
    res = specialize.curve.formula(curve$eq, xvar=curve$xvar,yvar=curve$yvar)
    
    c(curve, res)
  })
  restore.point("init.model.curves.end")

  invisible(em$curves)
}


