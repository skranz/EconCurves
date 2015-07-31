
examples.model.dependencies = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("GreenParadoxProgram")
  em = load.model("FiscalDebt")
  em = load.model("Capital3Eq")
  check.model(em)
  
  init.model(em,solve.systems = !TRUE)
  fo.df = em$fo.df
  sim = simulate.model(em)
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
  
  if (compute.par)
    compute.par.mat(em)

  em$var.mat = em$sim.fun(T = em$T,par.mat = em$par.mat,var.names = em$var.names)
  var.mat = em$var.mat
  par.mat = em$par.mat  
  T = em$T

  sim = var.par.to.sim(var.mat=var.mat, par.mat=par.mat)
  sim = add.sim.extra.vars(em,sim)
  em$sim = sim
  invisible(sim)
}

var.par.to.sim = function(var.mat, par.mat) {
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

fo.code.subst.li = function(fo.df,var.names=NULL, par.names = NULL) {
  restore.point("fo.code.subst.li")
  
  make.subst.li = function(names, df="var.mat", ti="ti", df.names=names) {
    subst = paste0(df,'[',ti,',"',df.names,'"]')
    li = lapply(subst, parse.as.call)
    names(li) = names
    li
  }
  if (is.null(var.names))
    var.names = fo.df$var
  li1 = make.subst.li(var.names, df = "var.mat",ti="ti")
  lag.var.names = paste0("lag_",var.names)
  li2 = make.subst.li(lag.var.names, df = "var.mat",ti="ti-1", df.names=var.names)
  
  if (is.null(par.names)) {
    par.names = setdiff(unique(do.call(c,fo.df$dependsOn)),c(var.names,lag.var.names))
    par.names = unique(str.right.of(par.names,"lag_"))
    par.names = unique(str.right.of(par.names,"lead_"))
  }

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
  ofo.df = em$ofo.df
  
  ofo.ti = function(period) {
    if (period == "main") {
      ti = quote(2:(T+1))
    } else if (period == "init") {
      ti = 2
    } else if (period == "laginit") {
      ti = 1
    }
    ti    
  }
  ofo.len = function(period) {
    if (period == "main") {
      len = quote(T)
    } else{
      len = 1
    }
    len   
  }

  
  subst.li = fo.code.subst.li(fo.df = ofo.df, var.names = em$var.names, par.names=em$par.names)
  
  # return value of function that nleqslv calls
  prog = em$prog
  constr_ = lapply(prog$constr_, substitute.call, env=subst.li )
  obj_ = substitute.call(prog$objective_, env=subst.li )

  names(constr_) = NULL

  # init x vector and specify its length
  is.len1 = sapply(ofo.df$period, function(per) per == "init" | per == "laginit")
  assign.len.x = substitute(len.x <- num.len1 + T * num.lenT, list(num.len1=sum(is.len1), num.lenT = sum(!is.len1)))
  init.x = substitute(start.x <- runif(len.x)) 
  

  all.lower = all(!sapply(ofo.df$lower, is.null))
  if (all.lower) {
    lower.init = quote(lower <- numeric(len.x))
  } else {
    lower.init=quote(lower<-NULL)
  }
  all.upper = all(!sapply(ofo.df$upper, is.null))
  if (all.upper) {
    upper.init = quote(upper <- numeric(len.x))
  } else {
    upper.init=quote(upper<-NULL)
  }

  lower.assign = upper.assign = NULL
  if (all.lower | all.upper) {
    lower.upper.assign = lapply(seq_along(ofo.df$var), function(i) {
      period = ofo.df$period[[i]]
      len = ofo.len(period)
      if (all.lower) {
        lower.call = substitute.call(ofo.df$lower[[i]], env=subst.li)
        lower.assign = substitute(lower[x.ind] <- lower.call, list(lower.call=lower.call))
      }
      if (all.upper) {
        upper.call = substitute.call(ofo.df$upper[[i]], env=subst.li)
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
  
  insert.x = lapply(seq_along(ofo.df$var), function(i) {
    period = ofo.df$period[[i]]
    ti = ofo.ti(period)
    len = ofo.len(period)
    substitute({
        x.ind <- x.start + 1:(len) 
        init.var.mat[ti,var] <- x[x.ind]
        x.start <- x.start + (len)
      },list(var=ofo.df$var[[i]],ti=ti, len=len))
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


create.sim.fun = function(em) {
  restore.point("create.sim.fun")
  if (NROW(em$ofo.df)==0) {
    em$sim.fun = make.inner.sim.fun(em)
  } else {
    em$inner.sim.fun = make.inner.sim.fun(em)
    em$sim.fun = make.outer.sim.fun(em,inner.sim.fun = em$inner.sim.fun)
  }
  invisible(em)
}

make.inner.sim.fun = function(em) {
  restore.point("make.inner.sim.fun")

  lifo.inner = make.inner.compute.code(fo.df=em$lifo.df, em=em)
  if (!is.null(em$ifo.df)) {
    fo.start = 3
    ifo.inner = make.inner.compute.code(fo.df=em$ifo.df, em=em)
  } else {
    fo.start = 2
    ifo.inner = NULL
  }
  
  fo.inner = make.inner.compute.code(fo.df=em$fo.df, em=em, lag.as.start=TRUE)

  fun.body = substitute(
    {
      #restore.point("inner.sim.fun")
      # init var.df matrix if it does not exist
      if (is.null(var.mat)) {
        nvar = length(var.names)
        var.mat = matrix(0,nrow=(T+2),ncol=nvar)
        colnames(var.mat) = var.names
      } 
      
      
      # Compute laginit
      ti = 1
      lifo.inner
      
      ti = 2
      ifo.inner
      
      for (ti in fo.start:(T+1)) 
        fo.inner
      
      return(var.mat)
    }, 
    list(lifo.inner=lifo.inner,ifo.inner=ifo.inner,fo.inner=fo.inner,fo.start=fo.start)
  )
  fun = function(T,par.mat, var.mat=NULL, var.names=NULL) {}
  body(fun) <- fun.body
  
  # compile function with byte code complier
  compfun = compiler::cmpfun(fun)
  
  compfun
}


make.inner.compute.code = function(fo.df,  var.names=em$var.names, par.names = em$par.names, em=NULL, all.implicit = FALSE, lag.as.start=FALSE) {
  restore.point("make.inner.compute.code")
  
  subst.li = fo.code.subst.li(fo.df, var.names,par.names)
  
  # example  
  fo = fo.df[1,]
  expl_ = fo$expl_[[1]]
  substitute.call(expl_,subst.li)
  
  clusters = sort(setdiff(unique(fo.df$cluster),NA))
  clu = 2
  li =lapply(clusters, function(clu) {
    rows = which(fo.df$cluster == clu)
    if (all(fo.df$solved[rows]) & (!all.implicit)) {
      code.li = lapply(rows, function(row) {
        code = substitute(lhs<-rhs, 
             list(lhs=as.symbol(fo.df$var[[row]]),rhs=fo.df$expl_[[row]]))
        code = fo.substitute.index.vars(code, em=em, var.names=var.names, par.names=par.names)
        code = substitute.call(code, subst.li)
        code
      })
      return(code.li)
    } else {
      endo = fo.df$var[rows]
      fo.df$impl_[rows]
      
      xsubst = lapply(seq_along(endo), function(i) {
        substitute(x[i],list(i=i))
      })
      names(xsubst) = endo

      # substitute original impl_ formula with x, par.mat and var.mat
      impl_ = lapply(fo.df$impl_[rows], function(call) {
        call = substitute.call(call, xsubst)
        call = substitute.call(call, subst.li)
        call
      })      
      inner = as.call(c(list(as.symbol("c")),impl_))
  
      assign = lapply(seq_along(endo), function(i) {
        substitute(var.mat[ti,var] <- res[[i]],list(i=i,var=endo[[i]]))
      }) 
      assign = as.call(c(list(as.symbol("{")),assign))
      
      if (lag.as.start) {
        #restore.point("ndjnfjdngjg")
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


fo.solve = function(fo.df, add.dep=FALSE, solve.systems=!TRUE) {
  restore.point("fo.solve")

  
  if (is.null(fo.df)) return(NULL)
  if (add.dep) {
    fo.df = fo.add.dependencies(fo.df)
  }
  
  clusters = setdiff(unique(fo.df$cluster),NA)
  fo.df$solved = FALSE
  for (clu in clusters) {
    rows = which(fo.df$cluster == clu)
    if (length(rows)==1) {
      row = rows
      if (!is.null(fo.df$expl_[[row]])) {
        fo.df$solved[row] = TRUE
        next
      }
      sol = sym.solve.eq(fo.df$eq_[[row]], fo.df$var[[row]])
      if (sol$solved) {
        fo.df$expl_[[row]] = sol$eq[[3]]
        fo.df$solved[row] = TRUE
      }
    } else {
      if (!solve.systems)
        next
      vars = fo.df$var[rows]
      eqs = fo.df$eq_[rows]
      
      restore.point("fo.solve.systems")
      
      sol = sym.solve.eqs(eqs=eqs, vars=vars)
      if (sol$solved) {
        rhs = lapply(sol$eqs, function(eq) eq[[3]])
        fo.df$expl_[rows] = rhs
        fo.df$solved[rows] = TRUE
      }
    }
  }
  fo.df
}

fo.add.dependencies = function(fo.df) {
  if (any(fo.df$outer))
    restore.point("fo.add.dependencies")
  res = dependency.graph(fo.df$dependsOn[!fo.df$outer], add.exo=FALSE)
  fo.df = left_join(fo.df, res$sym.df,by ="var")
  fo.df  
}

dependency.graph = function(dependsOn, add.exo=FALSE) {
  restore.point("dependencyGraph")

  library(igraph)  

  if (add.exo) {
    syms = unique(c(names(dependsOn), unlist(dependsOn)))
  } else {
    syms = names(dependsOn)
  }
  g <- graph.empty(directed=TRUE) + vertices(syms)
  for (i in seq_along(dependsOn)) {
    sym = names(dependsOn)[i]
    deps = intersect(dependsOn[[i]],syms)
    if (length(deps)>0) {
      g[from=deps,to=rep(sym,length(deps))] <- TRUE
    }
  }
  
  clusters = clusters(g,mode = "strong")$membership
  names(clusters) = syms
  
  li = lapply(sort(unique(clusters)), function(clu) {
    restore.point("njfndjnfbfzrba")
    csyms = syms[clusters==clu] 
    cvars = intersect(csyms, names(dependsOn))
    cdependsOn = setdiff(unique(unlist(dependsOn[cvars])),cvars)
    dependsOnClusters = clusters[intersect(cdependsOn,syms)]
    if (length(dependsOnClusters)==0) dependsOnClusters=NULL
    list(cluster=clu,size=length(cvars),vars=list(cvars), dependsOn = list(cdependsOn), dependsOnClusters=list(dependsOnClusters)) 
  })
  cluster.df = dplyr::as_data_frame(rbindlist(li))
  
  cluster.df$level = compute.dependsOn.levels(cluster.df$dependsOnClusters,use.names=FALSE)
  
  sym.df = data.frame(
    var=syms,
    cluster=clusters,
    level = cluster.df$level[clusters]
  )

  nlist(g,sym.df, cluster.df)
}

compute.dependsOn.levels = function(dependsOn, use.names=TRUE) {
  restore.point("compute.dependsOn.levels")
  
  levels = rep(NA_integer_, length(dependsOn))
  if (!use.names) {
    open = seq_along(dependsOn)
    counter = 0
    level = 1
    while(TRUE) {
      counter = counter+1
      has.level = sapply(open,function(ind) {
        length(intersect(dependsOn[[ind]],open))==0
      })
      levels[open[has.level]] = level
      open = open[!has.level]
      level = level+1
      if (length(open)==0) break
      if (counter>length(dependsOn))
        stop("dependsOn has cycles")
    }
    return(levels)
  }
  stop("Not yet implemented for names")  
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
  em$fo.df = fo.solve(em$fo.df,...)
  em$ifo.df = fo.solve(em$ifo.df,...)
  em$lifo.df = fo.solve(em$lifo.df,...)

}

init.model.vars = function(em) {
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
  
  
  em$fo.df = fo.add.dependencies(em$fo.df)
  if (!is.null(em$ifo.df))
    em$ifo.df = fo.add.dependencies(em$ifo.df)
  if (!is.null(em$lifo.df))
    em$lifo.df = fo.add.dependencies(em$lifo.df)

  em$par.names = find.em.par.names(em)
  #fo.df = em$fo.df
  #lifo.df = em$lifo.df
  
  invisible(em)
}

init.vars.formulas = function(em) {
  restore.point("init.vars.formulas")
  
  li.to.fo.ofo.df = function(li, period="main") {
    outer = sapply(li, function(fo) fo$type=="outer")  
    fo.df = dplyr::as_data_frame(rbindlist(li[!outer]))
    names(fo.df$eq_) = names(fo.df$expl_) = names(fo.df$impl_) = 
    names(fo.df$dependsOn) = fo.df$var
    
    ofo.df = dplyr::as_data_frame(rbindlist(li[outer]))
    if (NROW(ofo.df)>0) {
      ofo.df$period = period
      n = NROW(ofo.df)
#       if (period == "main") {
#         ofo.df$ti.call = replicate(n,quote(2:(T+1)),FALSE)
#         ofo.df$len.call = replicate(n,quote(T),FALSE)
#       } else if (period == "init") {
#         ofo.df$ti.call = replicate(n,quote(2),FALSE)
#         ofo.df$len.call = replicate(n,quote(1),FALSE)
#       } else if (period == "laginit") {
#         ofo.df$ti.call = quote(1)
#         ofo.df$len.call = quote(1)
#       }
    } else {
      ofo.df = NULL
    }
    list(fo.df=fo.df, ofo.df=ofo.df)
    
  }
  
  # phase t
  li = lapply(names(em$vars), function(var) {
    fo = init.var.formula(em,fo=em$vars[[var]],var=var, phase="t", list.wrap=TRUE)
    fo
  })
  res = li.to.fo.ofo.df(li, period="main")
  em$fo.df = res$fo.df
  ofo.df = res$ofo.df
  
  # For steady states
  lag.names = paste0("lag_",em$var.names)
  lag.subst.li = lapply(em$var.names,as.symbol)
  names(lag.subst.li) = lag.names  

  # phase init
  restore.point("init.vars.formulas.init")

  if (is.null(em$initMode)) 
    em$initMode = "like_t"
  
  no.init = sapply(em$vars, function(var) is.null(var[["init"]]))
  if (all(no.init) & em$initMode == "like_t") {
    em$init.like.t = TRUE
  } else {
    li = lapply(names(em$vars), function(var.name) {
      var = em$vars[[var.name]]
      fo = var$init
      steady.state = FALSE
      if (is.null(fo)) {
        if (em$initMode == "like_t") {
          fo = var
        } else if (em$initMode == "steady_state") {
          fo = var
          steady.state = TRUE
        } else {
          fo = list(formula="NA")
        }
      }
      fo = init.var.formula(em,fo=fo,var=var.name, phase="init", list.wrap=TRUE, steady.state = steady.state,lag.subst.li = lag.subst.li)
      fo
    })
    res = li.to.fo.ofo.df(li, period="init")
    em$ifo.df = res$fo.df
    ofo.df = rbind(ofo.df, res$ofo.df)

  }
    
  # phase laginit
  if (is.null(em$laginitMode)) em$laginitMode = "NA"
  
  var.name = names(em$vars)[[1]]
  li = lapply(names(em$vars), function(var.name) {
    restore.point("init.vars.formulas.laginit")

    var = em$vars[[var.name]]
    fo = var[["laginit"]]
    steady.state = FALSE
    if (is.null(fo)) {
      if (em$laginitMode == "like_t") {
        fo = var
      } else if (em$laginitMode == "steady_state") {
        fo = var
        steady.state = TRUE
      } else {
        fo = list(formula="NA")
      }
    } else if (!is.list(fo)) {
      fo = list(formula=fo)
    }
    fo = init.var.formula(em,fo=fo,var=var.name, phase="laginit", list.wrap=TRUE, steady.state = steady.state,lag.subst.li = lag.subst.li)
    fo
  })
  res = li.to.fo.ofo.df(li, period="laginit")
  em$lifo.df = res$fo.df
  ofo.df = rbind(ofo.df, res$ofo.df)

  if (NROW(ofo.df)>0) {
    cols = c("start", "lower", "upper")
    for (col in cols) ofo.df[[col]] = lapply(ofo.df[[col]], parse.as.call)
  }
  em$ofo.df = ofo.df
  em$has.outer = NROW(em$ofo.df) > 0

  invisible(em)
}

init.var.formula = function(em, fo, var=fo$name, phase=c("t","init","laginit")[1], list.wrap=FALSE, steady.state = FALSE, lag.subst.li=NULL) {
  restore.point("init.var.formula")
  
  if (steady.state) {
    lag.names = paste0("lag_",em$var.names)
    lag.subst.li = lapply(em$var.names,as.symbol)
    names(lag.subst.li) = lag.names  
  }
  
  type = get.var.formula.type(fo)
  if (type == "outer") {
    res = list(var=var,type=type,start=list(fo$start),lower=list(fo$lower),upper=list(fo$upper))
    return(res)
  }

  
  expl_ = NULL
  if (!is.null(fo$formula)) {
    expl_ = parse.as.call(fo$formula)
    if (steady.state)
      expl_ = substitute.call(expl_,lag.subst.li)
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

  if (steady.state) {
    eq_   = substitute.call(eq_,lag.subst.li)
  }
  
  impl_ = adapt.to.implicit.eq(eq_)
    
  #impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(eq_),rhs=get.rhs(eq_)))

  if (list.wrap) {
    res = list(var=var,type=type,expl_=list(expl_),impl_=list(impl_),eq_=list(eq_),outer=as.logical(is.true(fo$program)))
  } else {
    res = list(var=var,type=type,expl_=expl_,impl_=impl_,eq_=eq_,outer=as.logical(is.true(fo$program)))
  }
  
  dep.li = compute.var.dependsOn(em,var, impl_, list.wrap=list.wrap)
  
  c(res,dep.li)
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

compute.var.dependsOn = function(em,var, formula_ = em$impl_[[var]], list.wrap=FALSE) {
  dependsOn    = unique(setdiff(codeUtils::find.variables(formula_),var))

  if (list.wrap) {
    list(dependsOn=list(dependsOn))
  } else {
    nlist(dependsOn)
  }
}

find.em.par.names = function(em) {
  restore.point("find.em.par.names")
  all = unique(c("t","T",
                 unlist(em$fo.df$dependsOn),
                 unlist(em$ifo.df$dependsOn),
                 unlist(em$lifo.df$dependsOn),
                 names(em$params)
  ))
  if (length(em$scenarios)>0) {
    all = unique(c(all, names(em$scenarios[[1]]$params)))
  }

  
  pars = setdiff(all,c(em$var.names,paste0("lag_",em$var.names)))
  pars = unique(str.right.of(pars,"lag_"))
  pars = unique(str.right.of(pars,"lead_"))
  pars
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

  res = fo.substitute.index.vars(call,em=em)
  res
}


fo.substitute.index.vars = function(call, em, var.names=em$var.names, par.names=em$par.names) { 
  
  restore.point("fo.substitute.index.vars")
  
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
