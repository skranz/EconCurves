
examples.model = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("ThreeEq")
  em = load.model("ThreeEqFixedM")
  em = load.model("IS_LM_PC")
  em = load.model("AdaptivePricesRandom")
  
  init.model(em)
  init.model.scen(em)
  em$init.var
  em$sim = simulate.model(em,T = 200)
  c(sd(sim$p),sd(sim$p_adapt),sd(sim$p_fund))
  
  library(moments)
  c(kurtosis(sim$p),kurtosis(sim$p_adapt),kurtosis(sim$p_fund))
  
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


simulate.model = function(em,T=em$T, shocks=em$shocks, append=FALSE) {
  restore.point("simulate.model")

  compute.par.df(em,T = T,shocks = shocks) 
  
  all.par.df = cbind(em$par.df, em$lag.par.df, em$lead.par.df)
  # create lagged variables: are equal to current variables in steady state
  lag.var = c(em$init.var,em$init.randomVar)
  names(lag.var) = paste0("lag_",names(lag.var))

  
  t = 1
  res.li = vector("list")
  start = c(em$init.var, em$init.randomVar, lag.var, as.list(all.par.df[1,]))
  res.li[[t]] = start

  while(t < T) {
    t =  t+1
    res.li[[t]] = compute.next.period(t=t,em,prev=res.li[[t-1]],all.par.df=all.par.df)
    #sim = as.data.frame(rbindlist(res.li))
    #sim
  }
  sim = as_data_frame(rbindlist(res.li))
  
  cols = unique(c("t",names(em$init.var),names(em$init.randomVar), names(em$par.df),paste0("lag_",c(names(em$init.var),names(em$init.randomVar))),names(em$lag.par.df),names(em$lead.par.df)))
  em$sim = sim[,cols]
  
  invisible(em$sim)
}


compute.par.df = function(em, T, shocks) {
  restore.point("compute.par.df")
  
  par.df = cbind(data.frame(t=0:(T+1)), as.data.frame(em$init.par))
  
  for (shock in shocks) {
    shock.t = shock$start:(shock$start+shock$duration-1)
    shock.pars = names(shock$effects)
    for (par in shock.pars) {
      formula_ = parse.as.call(shock$effects[[par]])
      val = eval(formula_, par.df[shock.t+1,])
      par.df[[par]][shock.t+1] = val
    }
  }
  em$par.df      = par.df[2:(T+1),,drop=FALSE]
  em$lag.par.df  = par.df[1:(T),,drop=FALSE]
  em$lead.par.df = par.df[3:(T+2),,drop=FALSE]
  
  colnames(em$lag.par.df) = paste0("lag_", colnames(em$par.df))
  colnames(em$lead.par.df) = paste0("lead_", colnames(em$par.df))
      
}

make.nleq.fun = function(em, endo = em$var.names, exo = c(names(em$par.df),names(em$lag.par.df),names(em$lead.par.df)), impl_=em$impl_[endo]) {
  
  restore.point("make.nleq.fun")
  
  

  # replace all exogenous variables by exo[i]
  exo.sym.li = lapply(seq_along(exo), function(i) {
    substitute(exo[j], list(j=i))
  })
  names(exo.sym.li) = exo
  
  simpl_ = lapply(impl_, function(call) {
    substitute.call(call,exo.sym.li)
  })

  # replace all endogenous variables by x[i]
  endo.sym.li = lapply(seq_along(endo), function(i) {
    substitute(x[j], list(j=i))
  })
  names(endo.sym.li) = endo
  simpl_ = lapply(simpl_, function(call) {
    substitute.call(call,endo.sym.li)
  })


  code = paste0("function(x, exo) {\n",
    "  c(", paste0(sapply(simpl_,deparse,width.cutoff = 500L), collapse=","),")\n",
    "}"
  )
  fn = eval(parse(text=code))
  fn
  
}

compute.next.period = function(t,em, prev, all.par.df=em$all.par.df, round.digits=8) {
  restore.point("compute.next.period")
  
  vars = em$var.names
  pars = em$par.names
  
  prev = as.list(prev)

  act = prev
  act[em$lag.var.names] = prev[vars] 
  act[colnames(all.par.df)] = as.list(all.par.df[t,])
  act[["t"]] = t

  # evaluate random variables
  rvar = names(em$randomVars)[1]
  for (rvar in names(em$randomVars)) {
    act[[rvar]] = eval(em$randomVars[[rvar]]$formula_,act)
  }

  # Make code for nleqslv
  exo = setdiff(names(act), em$var.names)
  endo = em$var.names

  if (is.null(em$nleq.fun)) {
    em$nleq.fun = make.nleq.fun(em,endo=endo, exo=exo, impl_=em$impl_)
  }
  
  # start with previous values
  start.x = unlist(act[endo])
  exo.vals = unlist(act[exo])
  res = nleqslv(start.x,em$nleq.fun,exo=exo.vals)$x
  names(res) = endo
  if (!is.null(round))
    res = round(res,round.digits)
  
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
  init.model.randomVars(em)
  
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

init.model.scen = function(em,scen.name = names(em$scenarios)[1], scen = em$scenarios[[scen.name]]) {
  restore.point("init.model.scen")
  
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
  
  # init axis ranges
  em$panes = lapply(em$panes, function(pane) {
    restore.point("jndngjdng")
    pane$xrange = scen$axis[[pane$xvar]]
    pane$yrange = scen$axis[[pane$yvar]]
    pane
  })
  
  scen$init.par = as.list(env)
  em$scen = scen
  em$init.par = scen$init.par
  em$par.names = names(em$init.par)
  em$lag.par.names = paste0("lag_",em$par.names)
  
  em$shocks = scen$shocks
  init.model.shocks(em)
  
  em$T = scen$T
  
  compute.initial.period(em)
}


compute.initial.period = function(em, round.digits=10) {
  restore.point("compute.initial.period")
  par = em$init.par
  
  
  rvars = lapply(em$randomVars, function(var) NA_real_)
  exo.li = c(par,rvars)

  # init random variables
  for (rvar in names(rvars)) {
    exo.li[[rvar]] = eval(em$randomVars[[rvar]]$init_formula_,exo.li)
  }
  em$init.randomVar = exo.li[names(rvars)] 
  
  
  var.names = names(em$vars)
  
  fn = make.nleq.fun(em = em,endo = var.names,exo = names(exo.li),impl_ = em$impl_init_)
  
  # random start values on the unit interval
  x = runif(length(var.names))
  #x = rep(0,length(var.names))
  res = nleqslv(x,fn,exo=unlist(exo.li))$x
  
  names(res) = var.names
  if (!is.null(round))
    res = round(res, round.digits)
  em$init.var = as.list(res)
  invisible(em$init.var)
}

examples.find.model.par = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()

  em = load.model("IS_LM_PC")
  init.model(em)
  init.model.scen(em)
  es = load.story("IS_LM_PC_G_kurzfristig")

  find.model.par(em, find.par="M", extra.lhs='y_ - y_eq', scen=es$scenario)
  
}

find.model.par = function(em, find.par=NULL, extra.lhs=NULL, scen=NULL) {
  restore.point("find.model.par")
  
  if (!is.null(scen))
    init.model.scen(em,scen=scen)
  
  var.names = names(em$vars)
  par = em$init.par
  par.names = setdiff(names(par), find.par)
  par = par[par.names]
  find.par.ind =  seq_along(find.par) + length(var.names)
  impl_init_ = em$impl_init_
  cond.str = sapply(impl_init_,deparse1)
  cond.str = c(cond.str, extra.lhs)
  
  code = paste0("function(x) {\n",
#    'restore.point("fn")\n',
    sc("  ",names(par),"=",par, collapse="\n"),"\n\n",
    sc("  ",var.names,"=x[",seq_along(var.names),"]",
           collapse="\n"),"\n",
    sc("  ",find.par,"=x[",find.par.ind,"]",
           collapse="\n"),"\n",
    
    "  c(", paste0(cond.str, collapse=","),")\n",
    "}"
  )
  #cat(code)
  fn = eval(parse(text=code))
  fn
  # random start values on the unit interval
  x = runif(length(var.names)+length(find.par))
  res = nleqslv(x,fn)$x

  par.val = res[find.par.ind]  
    
  names(par.val) = find.par
  cat("\n\n",paste0(find.par,": ", par.val,collapse="\n"),"\n\n")
  par.val
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
  if (is.character(subs)) {
    subs = lapply(subs, function(s) {
      parse(text=s)[[1]]
    })
  }
  if (!is.list(subs)) {
    sub.li = list(subs)
  } else {
    sub.li = subs
  }
      
  names(sub.li) = var
  
  res = substitute.call(call, sub.li)
  #if (subset) res = res[[1]]
  res
}

init.model.vars = function(em) {
  restore.point("init.model.vars")

  # create types
  em$vars = lapply(em$vars, function(var) {
    restore.point("hfhhfuehuh")
    var$type = get.model.var.type(var)
    var$name = attr(var,"name")
    if (is.null(var$laginit))
      var$laginit = var$name
    var
  })

  
  # create explicit formulas
  em$expl_ = lapply(em$vars, function(var) {
    if (!is.null(var$formula))
      return(parse.as.call(var$formula))
    return(NULL)
  })
  
  # create explicit for initial values
  em$init_expl_ = lapply(em$vars, function(var) {
    if (!(var$laginit==var$name))
      return(parse.as.call(var$laginit))
    return(NULL)
  })
  

  em$vars = lapply(em$vars, function(var) {
    var
  })
  em$var.names = names(em$vars)
  em$lag.var.names = paste0("lag_",em$var.names)
  
  var.types = sapply(em$vars, function(var) var$type)
  em$var.types = var.types
  vars = em$vars
  # create single variable equations    
  
  rows = var.types %in% "xcurve" 
  xcurve.eq = lapply(vars[rows], function(var) {
    var$curve = var[[var$type]]
    curve = em$curves[[var$curve]]
    eq_ = subst.var(curve$eq_, var = curve$yvar, subs = var$y, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$xvar, subs = var$name, subset=FALSE)
    eq_
  })
    
  rows = var.types %in% "ycurve" 
  ycurve.eq = lapply(vars[rows], function(var) {
    var$curve = var[[var$type]]
    curve = em$curves[[var$curve]]
    eq_ = subst.var(curve$eq_, var = curve$xvar, subs = var$x, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$yvar, subs = var$name, subset=FALSE)
    eq_
  })

  rows = var.types %in% "formula" 
  formula.eq = lapply(vars[rows], function(var) {
    eq = paste0(var$name,"==",var$formula)
    eq_ = parse.as.call(eq)
    eq_
  })
  
  # we now need to find the equations for the cut points
  xrows = which(var.types == "xcut")
  yrows = which(var.types == "ycut")
  if (length(xrows) != length(yrows)) {
    stop("You must specify for every xcut variable exactly one ycut variable.")
  } 
  cuts.eq = NULL
  if (length(xrows)>0) {
    # Match xcuts and ycuts
    xcut.id = sapply(em$vars[xrows], function(var) paste0(var$xcut,collapse="."))
    ycut.id = sapply(em$vars[yrows], function(var) paste0(var$ycut,collapse="."))
  
    xdf = data_frame(xrow = xrows, id = xcut.id)
    ydf = data_frame(yrow = yrows, id = ycut.id)
    cut.df = inner_join(xdf,ydf,by="id")
    
    cuts.eq = lapply(int.seq(1,NROW(cut.df)), function(r) {
      restore.point("doirz8zhehf")
      xvar = vars[[cut.df$xrow[r]]]
      yvar = vars[[cut.df$yrow[r]]]
      curve.names = xvar$xcut
      eq.li = lapply(em$curves[curve.names], function(curve){
        eq_ = subst.var(curve$eq_, var = curve$xvar, subs = xvar$name, subset=FALSE)
        eq_ = subst.var(eq_, var = curve$yvar, subs = yvar$name, subset=FALSE)
        eq_
      })
      eq.li
    })
    cuts.eq = do.call(c,cuts.eq)
  }  
      

  eq.li = c(xcurve.eq, ycurve.eq, formula.eq, cuts.eq)
  
  impl.li = lapply(eq.li, function(eq_) {
    substitute(lhs-(rhs),list(lhs=get.lhs(eq_),rhs=get.rhs(eq_)))
  })  
  
  
  impl.init.li = lapply(impl.li, function(impl_) {
    restore.point("impl.init.li_38756")
    ivars = find.variables(impl_)
    lagged = ivars[str.starts.with(ivars,"lag_")]
    if (length(lagged)>0) {
      unlagged = str.right.of(lagged,"lag_")
      laginit = sapply(vars[unlagged], function(var) var$laginit)
      
      impl_init_ = subst.var(impl_,var = lagged, subs = laginit, subset=FALSE)
    } else {
      impl_init_ = impl_
    }
    impl_init_
  })

  em$impl_ = impl.li
  em$impl_init_ = impl.init.li
  invisible(em)
}


init.model.randomVars = function(em) {
  restore.point("init.model.randomVars")

  # create types
  em$randomVars = lapply(em$randomVars, function(var) {
    var$type = get.model.var.type(var)
    var$name = attr(var,"name")
    var$formula_ = parse.as.call(var$formula)
    
    if (is.null(var$laginit))
      var$laginit = var$formula
    var$init_formula_ = parse.as.call(var$laginit)
    
    var$dependsOn = find.variables(var$formula_)
    var
  })
  invisible(em)
}

get.model.var.type = function(var) {
  restore.point("get.model.var.type")
  types = c("xcurve","ycurve","formula","xcut","ycut")
  not.null = sapply(types, function(type) {!is.null(var[[type]])})
  types[not.null]
}



