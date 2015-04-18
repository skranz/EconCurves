
examples.model = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  res = load.model("ThreeEq")
  tt = res$tt
  em = res$em
  init.model(em)
  init.model.scen(em)
  #em$init.var
  em$sim = simulate.model(em)
  sim = em$sim
  dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)
}



simulate.model = function(em,T=10, shocks=em$shocks, append=FALSE) {
  restore.point("simulate.model")

  em$sim.shocks = shocks
  shocks.df = make.shocks.table(shocks)
  
  # create lagged variables: are equal to current variables in steady state
  lag.var = em$init.var
  names(lag.var) = paste0("lag_",names(em$init.var))

  lag.par = em$init.par
  names(lag.par) = paste0("lag_",names(em$init.par))

  t = 1
  res.li = vector("list")
  start = c(list(t=1),em$init.var, em$init.par, lag.var, lag.par)
  res.li[[t]] = start
  for (t in 2:T) {
    res.li[[t]] = compute.next.period(em,prev=res.li[[t-1]],shocks.df=shocks.df)
  }
  sim = as_data_frame(rbindlist(res.li))
  sim
}


compute.next.period = function(em, prev, shocks.df, round=10) {
  restore.point("compute.next.period")
  
  vars = em$var.names
  pars = em$par.names
  
  
  prev = as.list(prev)
  t = prev$t+1
  
  act = prev
  act[em$lag.par.names] = prev[pars] 
  act[em$lag.var.names] = prev[vars] 
  act[pars] = em$init.par
  act[["t"]] = t
  
  # Adapt parameters for current shocks
  shocks = shocks.df[which(shocks.df$t == t),,drop=FALSE]
  row = 0
  par = shocks$par[1]
  for (par in shocks$par) {
    row = row+1
    act[par] = eval(shocks$formula_[[row]], act)
  }
  
  # Make code for nleqslv
  exo = setdiff(names(act), em$var.names)
  endo = em$var.names
  
#   code = paste0("function(x) {\n",
#     paste0("  ",exo,"=",act[exo], collapse="\n"),"\n\n",
#     paste0("  ",endo,"=x[",seq_along(endo),"]", collapse="\n"),"\n",
#     "  c(", paste0(sapply(em$impl_[endo],deparse,width.cutoff = 500L), collapse=","),")\n",
#     "}"
#   )
  
  # replace all exogenous variables by their numerical values
  impl_ = lapply(em$impl_, function(call) {
    substitute.call(call,act[exo])
  })
  
  code = paste0("function(x) {\n",
    paste0("  ",endo,"=x[",seq_along(endo),"]", collapse="\n"),"\n",
    "  c(", paste0(sapply(impl_[endo],deparse,width.cutoff = 500L), collapse=","),")\n",
    "}"
  )
  #cat(code)
  fn = eval(parse(text=code))
  
  
  # start with previous values
  start.x = unlist(act[endo])
  res = nleqslv(start.x,fn)$x
  
  names(res) = endo
  if (!is.null(round))
    res = round(res)
  
  res = as.list(res)
  act[endo] = res  
  act   
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

init.model = function(em) {
  init.model.curves(em)
  init.model.panes(em)
  init.model.vars(em)
  init.model.shocks(em)
}


init.model.panes = function(em) {
  em$panes = lapply(em$panes, function(pane) {
    pane$name = attr(pane,"name")
    pane$xvar = pane$xy[1]
    pane$yvar = pane$xy[2]
    
    pane$xmarkers = lapply(pane$xmarkers, function(marker) {
      marker$name = get.name(marker)
      marker$axis = "x"
      marker
    })
    pane$ymarkers = lapply(pane$ymarkers, function(marker) {
      marker$name = get.name(marker)
      marker$axis = "y"
      marker
    })
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

init.model.scen = function(em, scen.name = names(em$scenarios)[1]) {
  restore.point("init.model.scen")
  
  scen = em$scenarios[[scen.name]]
  env = new.env()
  for (par in names(scen$init)) {
    val = scen$init[[par]]
    if (is.character(val)) {
      val = eval(parse(text=val),new.env)
    } else {
      attributes(val)=NULL
    }
    env[[par]] = val
  }
  scen$init.par = as.list(env)
  em$scen = scen
  em$init.par = scen$init.par
  em$par.names = names(em$init.par)
  em$lag.par.names = paste0("lag_",em$par.names)
  
  model.initial.var(em)
}


model.initial.var = function(em, round=8) {
  vars = names(em$vars) 
  par = em$init.par
  impl_ss_ = em$impl_ss_
  
  code = paste0("function(x) {\n",
    paste0("  ",names(par),"=",par, collapse="\n"),"\n\n",
    paste0("  ",vars,"=x[",seq_along(vars),"]", collapse="\n"),"\n",
    "  c(", paste0(sapply(impl_ss_,deparse,width.cutoff = 500L), collapse=","),")\n",
    "}"
  )
  cat(code)
  fn = eval(parse(text=code))
  
  x = rep(0,length(vars))
  res = nleqslv(x,fn)$x
  
  names(res) = vars
  if (!is.null(round))
    res = round(res)
  em$init.var = as.list(res)
  invisible(em$init.var)
}

# not yet implemented
make.model.jacobi = function(em) {
  ex = do.call(expression, em$impl)
  lapply(em$impl,function(call) {
    deriv(call,vars)
  })

}


init.model.curves = function(em) {
  restore.point("init.model.curves")
  
  curve = em$curves[[1]]
  em$curves = lapply(em$curves, function(curve) {
    curve$name = get.name(curve)
    curve$eq_ = parse.as.call(text=curve$eq)
    curve$impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(curve$eq_),rhs=get.rhs(curve$eq_)))
    
    curve$xvar = curve$xy[1]
    curve$yvar = curve$xy[2]
    
    res = specialize.curve.formula(curve$eq, xvar=curve$xvar,yvar=curve$yvar)
    
    c(curve, res)
  })
  invisible(em$curves)
}
subst.var = function(call, var, subs, subset=FALSE) {
  restore.point("substitute.variable")
  if (!is.character(var)) var = deparse(var)
  if (is.character(call)) call = parse(text=call)[[1]]
  if (is.character(subs)) subs = parse(text=subs)[[1]]
  
  sub.li = list(subs)
  names(sub.li) = var
  
  res = substitute.call(call, sub.li)
  #if (subset) res = res[[1]]
  res
}

init.model.vars = function(em) {
  var = em$variables[[1]]
  vars = lapply(em$vars, function(var) {
    restore.point("hfhhfuehuh")
    
    
    var$type = get.model.var.type(var)
    var$name = attr(var,"name")
    if (var$type == "xcurve" | var$type =="ycurve") {
      var$curve = var[[var$type]]
      curve = em$curves[[var$curve]]
      
      if (var$type == "xcurve") {
        var$eq_ = subst.var(curve$eq_, var = curve$yvar, subs = var$y, subset=FALSE)
        var$eq_ = subst.var(var$eq_, var = curve$xvar, subs = var$name, subset=FALSE)
      } else if (var$type == "ycurve") {
        var$eq_ = subst.var(curve$eq_, var = curve$xvar, subs = var$x, subset=FALSE)
        var$eq_ = subst.var(var$eq_, var = curve$yvar, subs = var$name, subset=FALSE)
      }
    } else if (var$type == "formula") {
      var$eq = paste0(var$name,"==",var$formula)
      var$eq_ = parse.as.call(var$eq)      
    }
    # Implicit equation
    var$impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(var$eq_),rhs=get.rhs(var$eq_)))

    
    # Condition for steady state
    ivars = find.variables(var$impl_)
    lagged = ivars[str.starts.with(ivars,"lag_")]
    if (length(lagged)>0) {
      unlagged = str.right.of(lagged,"lag_")
      var$impl_ss_ = subst.var(var$impl_,var = lagged, subs = unlagged, subset=FALSE)
    } else {
      var$impl_ss_ = var$impl_
    }
    
    var

  })
  
  
  em$vars = vars
  em$var.names = names(em$vars)
  em$lag.var.names = paste0("lag_",em$var.names)

  em$impl_ = lapply(em$vars, function(var) {var$impl_})
  em$impl_ss_ = lapply(em$vars, function(var) {var$impl_ss_})

  invisible(em)
}
get.model.var.type = function(var) {
  restore.point("get.model.var.type")
  types = c("xcurve","ycurve","formula")
  not.null = sapply(types, function(type) {!is.null(var[[type]])})
  types[not.null]
}

make.model.eq = function(em) {
  curves.eq = lapply(em$curves, function(curve) curve$eq)
  
  
}

run.model = function(em) {
  
}


