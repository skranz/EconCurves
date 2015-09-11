# Which equations and definitions to use?

# In period t>1: 
#     vars:     extract all equations defined under "vars" or "conditions"
#     lag_vars: take previous values
#     pars:     take initial values, possibly overwritten by a shock
# In the initial period: 
#     vars:     overwrite equations under "vars" if a special "init" condition is given
#     lag_vars: if steady state, set equal to init values unless explicitly overwritten. 
#               if a laginit condition is given under vars, add them to the conditions.
#               if lag_var is specified in scenario$init use it as init condition
#     pars:     In the initial period parameters may be endougenous as we can decide to fix
#               variables instead. The resulting value will be the default value for future periods
#
# Variable functions: will always be computed from period t>1 definitions, not from init definitions
#


examples.model.dependencies = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("Ger3Eq")
  em = load.model("SimpleLabor3Eq")
  check.model(em)
  options(warn=1)
  init.model(em)
  init.model.scen(em=em)
  
  cdf = em$cdf
  icdf = em$icdf
  sim = simulate.model(em,init.scen = FALSE)
  writeClipboard(paste0(deparse1(em$sim.fun,collapse = "\n")))
  
  inner.sim.report(em)
  
  em$sim.fun
  View(sim[1:10,])
  Rprof()
summaryRprof(tmp)
unlink(tmp)
 
}

simulate.model = function(em, scen.name = names(em$scenarios)[1], scen = em$scenarios[[scen.name]], init.scen=TRUE, compute.par=TRUE) {
  restore.point("simulate.model")
  
  if (!isTRUE(em$initialized)) 
    init.model(em)
  
  if (init.scen)
    init.model.scen(em,scen = scen)
  
  #if (compute.par)
  #  compute.par.mat(em)

  res = em$sim.fun(T = em$T,exo=em$init.exo, shocks=scen$shocks)
  var.mat = res$var.mat
  par.mat = res$par.mat  
  T = em$T

  sim = var.par.to.sim(T=T,var.mat=var.mat, par.mat=par.mat)
  sim = add.sim.extra.vars(em,sim)
  em$sim = sim
  invisible(sim)
}

var.par.to.sim = function(T,var.mat, par.mat) {
  restore.point("var.par.to.sim")
  
  par.df = par.mat[2:(T+1),,drop=FALSE]
  lag.par.df  = par.mat[1:(T),,drop=FALSE]
  lead.par.df = par.mat[3:(T+2),,drop=FALSE]
  
  colnames(lag.par.df) = paste0("lag_", colnames(par.mat))
  colnames(lead.par.df) = paste0("lead_", colnames(par.mat))

  var.mat = var.mat
  var.df = var.mat[2:(T+1),,drop=FALSE]
  lag.var.df = var.mat[1:(T),,drop=FALSE]
  colnames(lag.var.df) = paste0("lag_", colnames(var.mat))
  
  sim = as.data.frame(cbind(var.df,par.df,lag.var.df,lag.par.df,lead.par.df))
  sim  
}

add.sim.extra.vars = function(em, sim) {
  restore.point("add.sim.extr.vars")
  evs = em$extraVars
  if (length(evs)==0) return(sim)
  
  evs = sapply(evs, function(ev) ev$formula)
  code = paste0(names(evs),"=",evs, collapse=",\n")
  lag_code = paste0("lag_",names(evs),"=lag(",names(evs),")", collapse=",\n")

  code = paste0("mutate(sim,",code,",",lag_code,")")
  call = parse.as.call(code)
  new.sim = eval(call)
  new.sim
}


var.par.mat.subst.li = function(var.names=NULL, par.names = NULL) {
  restore.point("var.par.mat.subst.li")
  
  make.subst.li = function(names, df="var.mat", ti="ti", df.names=names) {
    subst = paste0(df,'[',ti,',"',df.names,'"]')
    li = lapply(subst, parse.as.call)
    names(li) = names
    li
  }
  li1 = make.subst.li(var.names, df = "var.mat",ti="ti")
  lag.var.names = paste0("lag_",var.names)
  li2 = make.subst.li(lag.var.names, df = "var.mat",ti="ti-1", df.names=var.names)

  li3 = make.subst.li(par.names, df = "par.mat",ti="ti")
  li4 = make.subst.li(paste0("lag_",par.names), df = "par.mat",ti="ti-1", df.names=par.names)
  li5 = make.subst.li(paste0("lead_",par.names), df = "par.mat",ti="ti+1",df.names=par.names)
  li6 = make.subst.li(paste0("lag_lead_",par.names), df = "par.mat",ti="ti",df.names=par.names)
  
  c(li1,li2,li3,li4,li5,li6)
}

init.program = function(em) {
  pr = em$program
  if (is.null(pr)) return(NULL)
  
  prog = list()
  
  if (!is.null(pr$minimize)) {
    prog$objective_ = parse.as.call(pr$minimize$formula, allow.null = FALSE)
    prog$goal = "min"
  } else if (!is.null(pr$maximize)) {
    prog$objective_ = parse.as.call(pr$maximize$formula, allow.null = FALSE)
    prog$goal = "max"
  } else {
    prog$objective_ = NULL
    prog$goal = "solve"
  }
  
  prog$org_constr_ = lapply(pr$constraints, function(con) {
    parse.as.call(con$formula)
  })
  prog$constr_ = lapply(prog$org_constr_, function(con) {
    if (prog$objective_ == NULL) {
      return(adapt.to.implicit.eq(con)) 
    } else {
      return(simple.constr.to.implicit(con))
    }
  })
  
  em$prog = prog
  invisible(prog)
}

make.outer.sim.fun = function(em, inner.sim.fun= em$inner.sim.fun, parent.env=parent.frame()) {
  restore.point("make.outer.sim.fun")
  init.program(em)
  ocdf = em$ocdf
  
  oc.ti = function(period) {
    if (period == "main") {
      ti = quote(2:(T+1))
    } else if (period == "init") {
      ti = 2
    } else if (period == "laginit") {
      ti = 1
    }
    ti    
  }
  oc.len = function(period) {
    if (period == "main") {
      len = quote(T)
    } else{
      len = 1
    }
    len   
  }

  
  subst.li = var.par.mat.subst.li(var.names = em$var.names, par.names=em$par.names)
  
  # return value of function that nleqslv calls
  prog = em$prog
  constr_ = lapply(prog$constr_, substitute.call, env=subst.li )
  obj_ = substitute.call(prog$objective_, env=subst.li )

  names(constr_) = NULL

  # init x vector and specify its length
  is.len1 = sapply(ocdf$period, function(per) per == "init" | per == "laginit")
  assign.len.x = substitute(len.x <- num.len1 + T * num.lenT, list(num.len1=sum(is.len1), num.lenT = sum(!is.len1)))
  init.x = substitute(start.x <- runif(len.x)) 
  

  all.lower = all(!sapply(ocdf$lower, is.null))
  if (all.lower) {
    lower.init = quote(lower <- numeric(len.x))
  } else {
    lower.init=quote(lower<-NULL)
  }
  all.upper = all(!sapply(ocdf$upper, is.null))
  if (all.upper) {
    upper.init = quote(upper <- numeric(len.x))
  } else {
    upper.init=quote(upper<-NULL)
  }

  lower.assign = upper.assign = NULL
  if (all.lower | all.upper) {
    lower.upper.assign = lapply(seq_along(ocdf$var), function(i) {
      period = ocdf$period[[i]]
      len = oc.len(period)
      if (all.lower) {
        lower.call = substitute.call(ocdf$lower[[i]], env=subst.li)
        lower.assign = substitute(lower[x.ind] <- lower.call, list(lower.call=lower.call))
      }
      if (all.upper) {
        upper.call = substitute.call(ocdf$upper[[i]], env=subst.li)
        upper.assign = substitute(upper[x.ind] <- upper.call, list(upper.call=upper.call))
      }
      
      substitute({
          x.ind <- x.start + 1:(len) 
          lower.assign
          upper.assign
          x.start <- x.start + (len)
        },list(len=len, lower.assign=lower.assign, upper.assign=upper.assign))
    }) 
    lower.upper.assign = as.call(c(list(as.symbol("{")),lower.upper.assign))
  } else {
    lower.upper.assign = NULL
  }
  
  insert.x = lapply(seq_along(ocdf$var), function(i) {
    period = ocdf$period[[i]]
    ti = oc.ti(period)
    len = oc.len(period)
    substitute({
        x.ind <- x.start + 1:(len) 
        init.var.mat[ti,var] <- x[x.ind]
        x.start <- x.start + (len)
      },list(var=ocdf$var[[i]],ti=ti, len=len))
  }) 
  insert.x = as.call(c(list(as.symbol("{")),insert.x))

  if (length(obj_)>0) {
    fn.assign = substitute(
      fn <- function(x,T,ti,par.mat, init.var.mat, inner.sim.fun) {
          x.start = 0
          insert.x
          var.mat = inner.sim.fun(T=T,par.mat=par.mat,var.mat=init.var.mat)
          (obj_)
      }, list(obj_=obj_,insert.x = insert.x )
    )
  } else {
    fn.assign = quote(fn <- NULL)
  }  

  if (length(constr_)>0) {
    criterion = as.call(c(list(as.symbol("c")),constr_))

    constr.assign = substitute(
      ineq.fn <- function(x,T,ti,par.mat, init.var.mat, inner.sim.fun) {
        x.start = 0
        insert.x
        var.mat = inner.sim.fun(T=T,par.mat=par.mat,var.mat=init.var.mat)
        
        #-(criterion)
        criterion
      }, list(criterion=criterion, insert.x=insert.x)
    )
  } else {
    constr.assign = quote(ineq.fn <- NULL)
  }  
  
  fun.body = substitute({
    # init outer variables with their guess value
    assign.len.x
    init.x
    
    # set ti=1 for lower and upper init
    ti=1
    lower.init
    upper.init
    x.start = 0
    lower.upper.assign
    
    # default set of rows
    ti = (2:(T+1))
    
    # init var.mat matrix if it does not exist
    if (is.null(var.mat)) {
      nvar = length(var.names)
      var.mat = matrix(0,nrow=(T+2),ncol=nvar)
      colnames(var.mat) = var.names
    } 
    
    init.var.mat = var.mat
    
    fn.assign
    constr.assign
    
    # solve 
    res = optims(par=start.x,fn=fn,ineq.fn = ineq.fn, T=T,ti=ti,par.mat=par.mat, init.var.mat=init.var.mat,inner.sim.fun=inner.sim.fun,lower=lower, upper=upper)
    if (!res$ok) warning("Outer search did not converge.")
    x = res$par
    
    x.start = 0
    insert.x
    var.mat = inner.sim.fun(T=T, par.mat=par.mat,var.mat=init.var.mat)
    var.mat
  },list(fn.assign=fn.assign,constr.assign=constr.assign,criterion=criterion, insert.x=insert.x, assign.len.x = assign.len.x, init.x=init.x, lower.init=lower.init, upper.init=upper.init, lower.upper.assign=lower.upper.assign)
  )
  
  fun = function(T,par.mat, var.mat=NULL, var.names=NULL) {}
  body(fun) <- fun.body
  
  
  fun.env = new.env(parent=parent.env)
  fun.env$inner.sim.fun = inner.sim.fun
  environment(fun) = fun.env  

  
  # compile function with byte code complier
  compfun = compiler::cmpfun(fun)
  
  compfun

}


#' Find partner variables for xcut and ycut formulas
init.formula.partners = function(em, fos) {
  restore.point("init.formula.partners")

  vars=names(fos)
  var.types = sapply(fos,get.var.formula.type)
  
  # we now need to find the equations for the cut points
  xrows = which(var.types == "xcut")
  yrows = which(var.types == "ycut")
  if (length(xrows) != length(yrows)) {
    stop("You must specify for every xcut variable exactly one ycut variable.")
  } 
  if (length(xrows)>0) {
    # Match xcuts and ycuts
    xcut.id = sapply(em$vars[xrows], function(var) paste0(var$xcut,collapse="."))
    ycut.id = sapply(em$vars[yrows], function(var) paste0(var$ycut,collapse="."))
  
    xdf = data_frame(xvar = vars[xrows], id = xcut.id)
    ydf = data_frame(yvar = vars[yrows], id = ycut.id)
    cut.df = inner_join(xdf,ydf,by="id")
    
    for (i in seq_along(xrows)) {
      fos[[cut.df$xvar[i]]]$y = cut.df$yvar[i]
      fos[[cut.df$yvar[i]]]$x = cut.df$xvar[i]
    }
  }
  fos
}

solve.model.explicit = function(em,...) {
  restore.point("solve.model.explicit")
  em$cdf = clu.solve(em$cdf,...)
  em$icdf = clu.solve(em$icdf,...)
  em$licdf = clu.solve(em$licdf,...)

}

init.model.vars = function(em, skip.cluster.equations=FALSE) {
  restore.point("init.model.vars")

  # create types
  em$vars = lapply(em$vars, function(var) {
    #restore.point("hfhhfuehuh")
    var$type = get.model.var.type(var)
    var$name = attr(var,"name")
    var
  })
  em$var.names = names(em$vars)

  em$vars = init.formula.partners(em,em$vars)
  init.vars.formulas(em)
  
  # Find all symbols
  sym.names = unique(c(em$var.names, 
    unlist(lapply(em$cdf$eq_, find.variables)),
    unlist(lapply(em$icdf$eq_, find.variables)),
    names(em$params), names(em$scenarios[[1]]$init)
  ))
  em$sym.names = sym.names[! (str.starts.with(sym.names,"lag_") | str.starts.with(sym.names,"lead_"))]
  
  
  
  em$par.names = setdiff(em$sym.names, em$var.names)

  em$org.cdf = em$cdf
  # solve equations
  #if (!skip.cluster.equations) {
    em$cdf = cluster.equations(em$org.cdf$eq_, endo=em$cdf$var, funs = em$var.funs)
  #}
  
  invisible(em)
}

init.vars.formulas = function(em) {
  restore.point("init.vars.formulas")
  
  li.to.cdf.ocdf = function(li, period="main") {
    restore.point("li.to.cdf.ocdf")
    if (length(li)==0) return(list(cdf=NULL, ocdf=NULL))
    outer = sapply(li, function(fo) fo$type=="outer")  
    cdf = dplyr::as_data_frame(rbindlist(li[!outer]))
    names(cdf$eq_) = names(cdf$expl_) = cdf$var 

    ocdf = dplyr::as_data_frame(rbindlist(li[outer]))
    if (NROW(ocdf)>0) {
      ocdf$period = period
      n = NROW(ocdf)
    } else {
      ocdf = NULL
    }
    list(cdf=cdf, ocdf=ocdf)
    
  }
  
  # phase t
  li = lapply(names(em$vars), function(var) {
    fo = init.var.formula(em,fo=em$vars[[var]],var=var, phase="t", list.wrap=TRUE)
    fo
  })
  res = li.to.cdf.ocdf(li, period="main")
  em$cdf = res$cdf
  ocdf = res$ocdf
  
  # phase init
  li = lapply(names(em$vars), function(var.name) {
    var = em$vars[[var.name]]
    fo = var$init
    if (!is.null(fo))
      fo = init.var.formula(em,fo=fo,var=var.name, phase="init", list.wrap=TRUE)
    lfo = var$laginit
    if (!is.null(lfo))
      lfo = init.var.formula(em,fo=fo,var=paste0("lag_",var.name), phase="init", list.wrap=TRUE)
    list(fo,lfo)
  })
  li = do.call(c,li)
  li = li[!sapply(li,is.null)]
    
  res = li.to.cdf.ocdf(li, period="init")
  em$icdf = res$cdf
  ocdf = rbind(ocdf, res$ocdf)
    

  if (NROW(ocdf)>0) {
    cols = c("start", "lower", "upper")
    for (col in cols) ocdf[[col]] = lapply(ocdf[[col]], parse.as.call)
  }
  em$ocdf = ocdf
  em$has.outer = NROW(em$ocdf) > 0

  invisible(em)
}

init.var.formula = function(em, fo, var=fo$name, phase=c("t","init"), list.wrap=FALSE) {
  restore.point("init.var.formula")

  type = get.var.formula.type(fo)
  if (type == "outer") {
    res = list(var=var,type=type,start=list(fo$start),lower=list(fo$lower),upper=list(fo$upper))
    return(res)
  }

  expl_ = NULL
  if (!is.null(fo$formula)) {
    expl_ = parse.as.call(fo$formula)
    eq_ = substitute(lhs==rhs, list(lhs=as.symbol(var),rhs=expl_))
  }

  if (type == "xcurve" | type =="ycurve") { 
    curve.name = fo[[type]]
    curve = em$curves[[curve.name]]
  }  
  
   
  if (type== "xcurve") { 
    eq_ = subst.var(curve$eq_, var = curve$yvar, subs = fo$y, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$xvar, subs = var, subset=FALSE)
  } else if (type=="ycurve") {
    eq_ = subst.var(curve$eq_, var = curve$xvar, subs = fo$x, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$yvar, subs = var, subset=FALSE)
  } else if (type=="formula") {
    eq_ = substitute(var == expl, list(var=as.name(var), expl=expl_))
  } else if (type=="xcut") {
    xvar = var
    yvar = fo$y
    curve.name = fo[[type]][1] # xcut shall be first curve
    curve = em$curves[[curve.name]]
    eq_ = subst.var(curve$eq_, var = curve$xvar, subs = xvar, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$yvar, subs = yvar, subset=FALSE)
  } else if (type=="ycut") {
    yvar = var
    xvar = fo$x
    curve.name = fo[[type]][2] # ycut shall be second curve
    curve = em$curves[[curve.name]]
    eq_ = subst.var(curve$eq_, var = curve$xvar, subs = xvar, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$yvar, subs = yvar, subset=FALSE)
  } else if (type=="eq") {
    eq_ = parse.as.call(fo$eq)
  }

  if (list.wrap) {
    res = list(var=var,type=type,expl_=list(expl_),eq_=list(eq_),outer=as.logical(is.true(fo$program)))
  } else {
    res = list(var=var,type=type,expl_=expl_,eq_=eq_,outer=as.logical(is.true(fo$program)))
  }

  res
}


get.var.formula.type = function(fo) {
  restore.point("get.var.formula.type")
  types = c("xcurve","ycurve","formula","xcut","ycut","eq","outer")
  not.null = sapply(types, function(type) {!is.null(fo[[type]])})
  type = types[not.null]
  if (length(type)==0) {
    stop(paste0("Cannot deduce variable type for ", get.name(fo),"."))
  }
  type
}


compute.par.mat = function(em, T=em$T, shocks=em$shocks, seed=NULL) {
  restore.point("compute.par.df")
  scen = em$scen
  
  if (!is.null(seed))
    set.seed(seed)
  
  # Init parameter list
  par.li = em$init.par
  par.li[names(scen$init.par)] = scen$init.par
  par.li$T=as.numeric(T)
  par.li$t=0:(T+1)
  
  #par.li$.ti = 2:(T+1)
  
  
  for (i in seq_along(par.li)) {
    if (is.call(par.li[[i]])) {
      par.li[[i]] = eval(par.li[[i]], par.li)
    }
  }
  #par.li = par.li[setdiff(colnames(par.li),".ti")]
  
  par.df = as.data.frame(par.li)

  # add shocks  
  for (shock in shocks) {
    if (shock$start>T+1) next
    
    shock.t = shock$start:min(T+1,(shock$start+shock$duration-1))
    shock.pars = names(shock$effects)
    for (par in shock.pars) {
      formula_ = parse.as.call(shock$effects[[par]])
      val = eval(formula_, par.df[shock.t+1,])
      par.df[[par]][shock.t+1] = val
    }
  }
  em$par.mat = as.matrix(par.df)
  invisible(em$par.mat)
}


get.model.var.type = function(var) {
  restore.point("get.model.var.type")
  types = c("xcurve","ycurve","formula","xcut","ycut")
  not.null = sapply(types, function(type) {!is.null(var[[type]])})
  types[not.null]
}

examples.substitute.index.var = function() {
  call = quote(4+eps[1:z])
  replace.pattern = quote(ti+(old.ind))

  res = clu.substitute.index.vars(call,em=em)
  res
}


clu.substitute.index.vars = function(call, em, var.names=em$var.names, par.names=em$par.names) { 
  
  restore.point("clu.substitute.index.vars")
  
  recursive.fun = function(call) {
    if (length(call)<=1) return(call)
    if (call[[1]]=="[") {
      old.ind = call[[3]]
      sym.name = as.character(call[[2]])
      sym.name = str.right.of(sym.name,"lag_")
      sym.name = str.right.of(sym.name,"lead_")
      sym.name = str.right.of(sym.name,"lag_lead_")
      
      if (sym.name %in% par.names) {
        ind = substitute(bound.value(ti+(old.ind),1,T+2),list(old.ind=old.ind))
        call = substitute(par.mat[ind, sym.name], list(ind=ind, sym.name=sym.name))
      } else {
        ind = substitute(bound.value(ti+(old.ind),1,T+1),list(old.ind=old.ind))
        call = substitute(var.mat[ind, sym.name], list(ind=ind, sym.name=sym.name))
      }
    } else {
      for (i in seq_along(call)) {
        call[[i]] = recursive.fun(call[[i]])
      }
    }
    call
  }    
  
  recursive.fun(call)
}

make.init.eqs.and.exo = function(em, scen=em$scen,steady.state = is.true(scen$init_mode=="steady_state") ) {
  restore.point("make.init.eqs.and.exo")
  
  init = scen$init
  init.syms = names(init)
  
  vars = em$var.names
  pars = em$par.names
  
  oeqs = em$cdf$org_
  
  # need to overwrite equations for which init is explicitly defined
  
  # Find lag variables that will be replaced
  # by their non-lag counterpart, since they are
  # not in the steady state
  if (steady.state) {
    syms = c(vars, pars)
    lag.syms = paste0("lag_", syms)
    replace.lag = setdiff(syms, init.syms)
    rows = which(lag.syms %in% replace.lag)
    
    replace.li = lapply(syms[rows], function(sym) as.name(sym))
    names(replace.li) = lag.syms[rows]
  
    # replace lags in equations  
    roeqs = lapply(oeqs, function(eq_) {
      substitute.call(eq_, replace.li)
    })
  } else {
    roeqs = oeqs
  }

  # Extract exogenously initialized symbols and init equations
  number.init= init.syms[sapply(init, function(val)  is.numeric(val))]   
  exo = number.init
  endo.init = setdiff(init.syms, exo)
  init.eqs = lapply(endo.init, function(var) {
    rhs = parse.as.call(init[[var]])
    substitute(lhs == rhs, list(lhs=as.name(var),rhs=rhs))
  })
  names(init.eqs) = endo.init
  
  em$init.eqs = c(init.eqs, roeqs)
  em$init.exo = sapply(init[number.init], as.numeric)
    
}

make.init.cluster.df = function(em, scen=em$scen, steady.state = is.true(scen$init_mode=="steady_state")) {
  restore.point("make.init.cluster.df")
  
  
  make.init.eqs.and.exo(em=em,scen=scen, steady.state=steady.state)
  
  #eqs.solution.report(em$init.eqs, exo=em$init.exo, as.numeric), funs=em$var.funs)
  
  res.df = cluster.equations(em$init.eqs, exo=names(em$init.exo), funs=em$var.funs)
  res = extract.cluster.df.dummies(res.df)
  em$icdf = res$df 
  em$init.test.eqs = res$test.eqs

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
  

  return(invisible())
}


create.sim.fun = function(em) {
  restore.point("create.sim.fun")
  if (NROW(em$ocdf)==0) {
    em$sim.fun = make.inner.sim.fun(em)
    
  } else {
    em$inner.sim.fun = make.inner.sim.fun(em)
    em$sim.fun = make.outer.sim.fun(em,inner.sim.fun = em$inner.sim.fun)
  }
  invisible(em)
}

make.inner.sim.fun = function(em) {
  restore.point("make.inner.sim.fun")

  init.compute.code = make.init.compute.code(em=em)
  clu.inner = make.inner.compute.code(cdf=em$cdf, em=em, lag.as.start=TRUE)

  fun.body = substitute(
    {
      restore.point("inner.sim.fun")
      
      init.compute.code

      # init parameters, including shocks
      par.li = c(list(t=1:(T+2),T=T),as.list(par.mat[2,]))
      names(par.li) = c("t","T",colnames(par.mat))      
      par.df = as.data.frame(par.li)

      # add shocks  
      for (shock in shocks) {
        if (shock$start>T+1) next
        
        shock.t = shock$start:min(T+1,(shock$start+shock$duration-1))
        shock.pars = names(shock$effects)
        for (par in shock.pars) {
          formula_ = parse.as.call(shock$effects[[par]])
          val = eval(formula_, par.df[shock.t+1,])
          par.df[[par]][shock.t+1] = val
        }
      }
      par.mat = rbind(c(t=1,T=T,par.mat[1,]),as.matrix(par.df))

      for (ti in 3:(T+1)) 
        clu.inner
      
      return(list(par.mat=par.mat, var.mat=var.mat))
    }, 
    list(clu.inner=clu.inner, init.compute.code=init.compute.code)
  )
  fun = function(T,exo=NULL, shocks=NULL) {}
  body(fun) <- fun.body
  
  # compile function with byte code complier
  compfun = compiler::cmpfun(fun)
  compfun
}


make.init.compute.code = function(icdf=em$icdf,exo = names(em$init.exo), em=NULL) {
  restore.point("make.init.compute.code")
 
  par.names = em$par.names
  var.names = em$var.names
  
  subst.li = var.par.mat.subst.li(var.names,par.names)
  
  li = lapply(exo, function(sym) {
    call = substitute(sym <- exo[[sym.name]], list(sym=as.name(sym), sym.name=sym))
    substitute.call(call,subst.li)
  }) 
  exo.assign = as.call(c(list(as.symbol("{")),li))
  
  inner = make.inner.compute.code(cdf=icdf, em=em)
  
  code = substitute({
      par.mat = matrix(NA_real_,T+3,npar)
      colnames(par.mat) = par.names
      var.mat = matrix(NA_real_,T+2,nvar)
      colnames(var.mat) = var.names
      
      ti = 2
      
      exo.assign
      inner
      
    },
    list(
      par.names=par.names, var.names = var.names,
      nvar=length(var.names), npar=length(par.names),
      exo.assign = exo.assign, inner = inner
    )
  )
  return(code)
  
  #body(fun) <- code
  #compfun = compiler::cmpfun(fun)
  #compfun
}


make.inner.compute.code = function(cdf,  var.names=em$var.names, par.names = em$par.names, em=NULL, all.implicit = FALSE, lag.as.start=FALSE, subst.li = var.par.mat.subst.li(var.names,par.names) ) {
  restore.point("make.inner.compute.code")


  clusters = unique(cdf$cluster)
  clu = 2
  li =lapply(clusters, function(clu) {
    rows = which(cdf$cluster == clu)
    if (all(cdf$solved[rows]) & (!all.implicit)) {
      code.li = lapply(rows, function(row) {
        code = substitute(lhs<-rhs, 
             list(lhs=as.symbol(cdf$var[[row]]),rhs=cdf$expl_[[row]]))
        code = clu.substitute.index.vars(code, em=em, var.names=var.names, par.names=par.names)
        code = substitute.call(code, subst.li)
        code
      })
      return(code.li)
    } else {
      restore.point("kjdkjflkdjghdfoighfh")
      endo = cdf$var[rows]

      xsubst = lapply(seq_along(endo), function(i) {
        substitute(x[i],list(i=i))
      })
      names(xsubst) = endo

      # substitute original impl_ formula with x, par.mat and var.mat
      impl_ = lapply(cdf$eq_[rows], function(call) {
        call = substitute(lhs-(rhs), list(lhs=call[[2]],rhs=call[[3]]))
        call = substitute.call(call, xsubst)
        call = substitute.call(call, subst.li)
        call
      })      
      inner = as.call(c(list(as.symbol("c")),impl_))
  
      assign = lapply(seq_along(endo), function(i) {
        substitute(var <- res[[i]],list(i=i,var=subst.li[[ endo[[i]] ]]))
      }) 
      assign = as.call(c(list(as.symbol("{")),assign))
      
      if (lag.as.start) {
        rhs = lapply(seq_along(endo), function(i) {
          substitute(var.mat[ti-1,var],list(var=endo[[i]]))
        }) 
        rhs = as.call(c(list(as.symbol("c")),rhs))

        start = substitute(start.x <- rhs, list(rhs=rhs)) 
      } else {
        start = substitute(start.x <- runif(num.endo), list(num.endo=length(endo))) 
      }
      code = substitute({
          start
          sol = mynleqslv(x=start.x,ti=ti,par.mat=par.mat, var.mat=var.mat,
            fn = function(x,ti,par.mat, var.mat) {
            inner
          })
          if (max(abs(sol$fvec))> 1e-07) {
            warning(paste0("Could not solve ", paste0(endo, collapse=", "), " for ti = ", paste0(ti, collapse=", ")," max deviation = ",max(abs(sol$fvec)) ))
          }
          res = sol$x
          assign
        },
        list(inner=inner, start=start,assign=assign,endo=endo)
      )
      list(code)  
    }
    
  })
  li = do.call(c, li)

  inner = as.call(c(list(as.symbol("{")),li))
  inner  
}


