
examples.steady.state = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("Capital3Eq")
  em = load.model("General3Eq")
  check.model(em)
  
  init.model(em,solve.systems = !TRUE)
  init.model.scen(em = em)
  res = make.init.cluster.df(em)
  
  res = solve.steady.state(em)
  clu.df = res$clu.df
  exo = res$exo
  sim = simulate.model(em)
  

}

make.init.cluster.df = function(em, scen=em$scen, steady.state = TRUE) {
  restore.point("make.init.cluster.df")
  
  init = scen$init
  init.syms = names(init)
  
  so.df = em$fo.df
  vars = so.df$var
  pars = em$par.names
  
  # Find lag variables that will be replaced
  # by their non-lag counterpart, since they are
  # not in the steady state
  
  if (steady.state) {
    so.syms = unique(unlist(lapply(so.df$eq_, function(eq_) find.variables(eq_))))
    so.lag = so.syms[str.starts.with(so.syms,"lag_")]
    replace.lag = setdiff(so.lag, init.syms)
    
    replace.li = lapply(replace.lag, function(sym) as.name(str.right.of(sym,"lag_")))
    names(replace.li) = replace.lag
  
    # replace lags  
    so.eqs = lapply(so.df$eq_, function(eq_) {
      substitute.call(eq_, replace.li)
    })
  } else {
    so.eqs = so.df$eq_
  }
  names(so.eqs) = so.df$var

  # Extract exogenously initialized symbols and init equations
  number.init= init.syms[sapply(init, function(val)  is.numeric(val))]   
  exo = number.init
  endo.init = setdiff(init.syms, exo)
  init.eqs = lapply(endo.init, function(var) {
    rhs = parse.as.call(init[[var]])
    substitute(lhs == rhs, list(lhs=as.name(var),rhs=rhs))
  })
  names(init.eqs) = endo.init
  
  eqs = c(init.eqs, so.eqs)
  
  funs = extract.explicit.eqs(eqs)
  
  #eqs.solution.report(eqs, exo=sapply(init[number.init], as.numeric), funs=funs)
  clu.df = cluster.equations(eqs, exo=exo, funs=funs, skip.big=skip.big)
  clu.df

  
  if (FALSE) {  
    txt = cluster.df.nice.code(clu.df)
    txt = c(
      "# Init exogenous symbols",
      paste0(number.init," = ", init[number.init],collapse="; " ),
      txt
    )
    txt = paste0(txt, collapse="\n\n")
    writeClipboard(txt)
  }    
  
  list(clu.df=clu.df, exo = init[number.init])
}



# 
# make.solve.fun = function(expl.li, eq.li) {
#   expl.dep = lapply(expl.li, function(call) find.variables(call))
#   eq.dep = lapply(eq.li, function(call) find.variables(call))
#   
#   expl = names(expl.li)
#   eq = names(eq.li)
#   
# 
#   
#   res = dependency.graph(c(expl.dep,eq.dep))
#   clu.df = res$sym.df
# 
#   fo.df = data_frame(var=c(expl,eq), expl_=c(expl.li, vector("list", length(eq.li))), eq_= c(vector("list", length(expl.li)), eq.li))
#   
#   fo.df = inner_join(fo.df, clu.df, by="var")
# 
#   # copy code from model_computer...  
# }
# 


large.solve.steady.state = function(em, scen=em$scen) {
  restore.point("solve.steady.state")
  
  init = scen$init
  
  syms = names(init)
  
  
  so.df = em$fo.df
  vars = so.df$var
  pars = em$par.names
  
  # Find lag variables that will be replaced
  # by their non-lag counterpart, since they are
  # not in the steady state
  so.syms = unique(unlist(lapply(so.df$eq_, function(eq_) find.variables(eq_))))
  so.lag = so.syms[str.starts.with(so.syms,"lag_")]
  replace.lag = setdiff(so.lag, syms)
  
  replace.li = lapply(replace.lag, function(sym) as.name(str.right.of(sym,"lag_")))
  names(replace.li) = replace.lag

  # replace lags  
  eq.li = lapply(so.df$eq_, function(eq_) {
    substitute.call(eq_, replace.li)
  })
  names(eq.li) = so.df$var

  # All equations will be translated to implicit conditions
  impl1_ = lapply(eq.li, function(eq_) substitute(lhs-(rhs), list(lhs=eq_[[2]],rhs=eq_[[3]])))
  
  # All formula parameters will be translated to implicit conditions
  form.init= syms[sapply(init, function(val)  is.character(val))]   
  impl2_ = lapply(form.init, function(sym) {
    form_ = parse.as.call(init[[sym]])
    substitute(sym - (form_), list(sym=as.name(sym), form_=form_))
  })    
  impl_ = c(impl1_,impl2_)
  
  # Symbols that need to be solved
  solve.syms = unique(c(
    form.init,
    setdiff(c(vars,pars),c("T","t",syms))
  ))
  
  length(impl_)
  length(solve.syms)
  
  solve.pars = intersect(solve.syms, pars)
  solve.vars = intersect(solve.syms, vars)
  solve.pars
  solve.vars
  
  if (length(impl_) != length(solve.syms)) {
    msg = paste0("To compute the steady state, you want to solve ", length(solve.syms), " symbols but have ", length(impl_)," equations. The variables and parameters you want to solve are...",
      "\npars: ",paste0(solve.pars, collapse=", "),
      "\nvars: ",paste0(solve.vars, collapse=", ")
    )
    #cat(msg)
    warning(msg)
  } else {
    msg = ""
  }
  
  endo = solve.syms
  exo = setdiff(syms,solve.syms)
  endo.subst = lapply(seq_along(endo), function(i) {
    substitute(x[i],list(i=i))
  })
  names(endo.subst) = endo

  exo.list = lapply(init[exo], function(val) as.numeric(val))
  
  #substitute original impl_ formula with x, par.mat and var.mat
  simpl_ = lapply(impl_, function(call) {
    call = substitute.call(call, c(endo.subst,exo.list))
    call
  })
  names(simpl_) = NULL
#   if (length(solve.syms)>length(simpl_)) {
#     zero.cond 0 
#     
#   }
  inner = as.call(c(list(as.symbol("c")),simpl_))
 
  fn = function(x) {}
  body(fn) = inner
  fn
  
  # solve equations
  x <- runif(length(endo)) 
#  sol = mynleqslv(x=x, fn=fn)
#  if (max(abs(sol$fvec))> 1e-07) {
#    warning(paste0("Could not solve ", paste0(endo, collapse=", ")," in steady state: max deviation = ",max(abs(sol$fvec)) ))
#  }
#  x = sol$x

  x <- runif(length(endo)) 
  sum.fn = function(x) sum(fn(x)^2)
  sum.fn(x)
  sol = optims(par = x, fn = sum.fn)
  sol$ok
  sol
  
  
  library(nloptr)
  x <- runif(length(endo)) 
  n = length(endo)
  sol = stogo(x, sum.fn, lower = rep(-1000,n), upper=rep(1000,n), nl.info=TRUE)
  
  directL(sum.fn, lower, upper, randomized = FALSE, original = FALSE,
        nl.info = FALSE, control = list(), ...)
  
  endo.list = as.list(x)
  names(endo.list) = endo
  

  c(exo.list, endo.list)
  
}


mysolve = function(x, fn) {

  sol = optims(x,eq.fn = fn)
  if (!sol$solved) {
    sol$solved = FALSE  
    warning(paste0("Could not solve ", paste0(c("w_max", "C"), collapse = ", "), " in steady state: max deviation = ", max(abs(sol$fvec))))
  } else {
    sol$solved = TRUE
  }

  if (!sol$solved) {
    
  }
}
