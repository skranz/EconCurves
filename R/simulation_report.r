examples.inner.sim.report = function(em) {
  inner.sim.report(em)
}


inner.sim.report = function(em, id=em$modelId, dir=getwd()) {
  rep = new.report.writer()
  
  if (is.null(em$var.mat)) {
    nvar = length(em$var.names)
    var.mat = matrix(0,nrow=(em$T+2),ncol=nvar)
    colnames(var.mat) = var.names
    em$var.mat = var.mat
  } 

  copy.into.env(em)
  rep$w('
{{id}}.simulation.report = function(em) {

  copy.into.env(em)

  nvar = length(em$var.names)
  var.mat = matrix(NA_real_,nrow=(em$T+2),ncol=nvar)
  colnames(var.mat) = var.names
  em$var.mat = var.mat

')

  for (ti in 1:NROW(em$var.mat)) {
    ti.report(ti,em,rep)    
  }

  rep$w("}")
  str = paste0(rep$txt, collapse="\n")
  cat(str)
  writeClipboard(str)
}



ti.report = function(ti=1,em,rep) {
  restore.point("ti.report")
rep$w('
  
  #####################################################
  ## Period t={{ti-1}}  
  #####################################################
  
  ti = {{ti}}
  assign.from.var.par(ti, var.mat, par.mat)

  
')
  if (ti == 1) {
    fo.df = em$lifo.df
  } else if (ti == 2) {
    fo.df = em$ifo.df
  } else {
    fo.df = em$fo.df
  }
    
  clusters = sort(setdiff(unique(fo.df$cluster),NA))
  clu = 2
  for (clu in clusters) {
    report.ti.cluster(clu,fo.df, em, rep)
  }
  rep$w("  var.mat = assign.to.var.mat(ti=ti, var.mat)")
}

report.ti.cluster = function(clu,fo.df,em,rep) {
  restore.point("report.ti.cluster")
  rows = which(fo.df$cluster == clu)
  if (all(fo.df$solved[rows])) {
    for (row in rows) {
      code = substitute(lhs<-rhs, 
           list(lhs=as.symbol(fo.df$var[[row]]),rhs=fo.df$expl_[[row]]))
      rep$w("  ",deparse1(code))
    }
  } else {
    endo = fo.df$var[rows]
    endo_str = paste0(endo, collapse=", ")
    impl_ = fo.df$impl_[rows]       
    impl_str = paste0("    # ",endo, "\n    ",sapply(impl_,deparse1), collapse=", \n")
    x.as.var = paste0(endo,"=x[",seq_along(endo),"]", collapse=";")
    assign = paste0("  ",endo," =x[",seq_along(endo),"]", collapse=";")

    rep$w('
  # Now we solve {{endo_str}}
  
  # Start parameters
  x = runif({{length(endo)}})
  
  fn = function(x,ti,par.mat, var.mat) {
    {{x.as.var}}
    c(
  {{impl_str}}
    )
  }
  fn(x,ti,par.mat,var.mat)  

  # Show panes
  sim = report.make.sim()
  plot.pane(em = em,pane = 1,sim=sim,t=ti-1)

  sol = mynleqslv(x=x,fn=fn,ti=ti,par.mat=par.mat, var.mat=var.mat)
  
  if (max(abs(sol$fvec))> 1e-07) {
    warning(paste0("Could not solve {{endo_str}} for ti = ", paste0(ti, collapse=", ")," max deviation = ",max(abs(sol$fvec)) ))
  }
  x = sol$x
  x
  # Assign to var.mat
  {{assign}}
    ')      
  }  
}

assign.from.var.par = function(ti=1, var.mat, par.mat, env =parent.frame()) {
  
  vars = colnames(var.mat)
  for (var in vars) {
    env[[var]] = var.mat[ti,var]
    if (ti > 1)
      env[[paste0("lag_",var)]] = var.mat[ti-1,var]
    if (ti < NROW(var.mat))
      env[[paste0("lead_",var)]] = var.mat[ti+1,var]
  }
  pars = colnames(par.mat)
  for (par in pars) {
    env[[par]] = par.mat[ti,par]
    if (ti > 1)
      env[[paste0("lag_",par)]] = par.mat[ti-1,par]
    if (ti < NROW(par.mat))
      env[[paste0("lead_",par)]] = par.mat[ti+1,par]
  }
  
}

assign.to.var.mat = function(ti=1, var.mat, env =parent.frame()) {
  
  vars = colnames(var.mat)
  for (var in vars) {
    var.mat[ti,var] = env[[var]] 
  }
  var.mat
}

report.make.sim = function(env =parent.frame()) {
  em = env$em
  restore.point("report.make.sim")
  var.mat = env$var.mat
  ti = env$ti
  var.mat = assign.to.var.mat(ti,var.mat,env)
  par.mat = env$par.mat
  
  sim = var.par.to.sim(var.mat=var.mat, par.mat=par.mat)
  em$sim = sim
  invisible(sim)
}


new.report.writer = function() {
  rep = as.environment(list(txt=NULL))
  rep$w = function(..., env=parent.frame()) {
    str = paste0(...)
    str = replace.whiskers(str,env)
    rep$txt <- c(rep$txt,str)
  }
  rep
}
