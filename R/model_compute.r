# To speed up computation, it is helpful to categorize variables in different dimensions:
#
# init vs laginit vs t:
#     we perform separate categorizations for the computation of the 
#     initial value, the lagged initial value and the period t value
#     of a variable.
#
# explicit vs implicit: 
#     do we have an explicit formula or just an implicit formula
#
# inner vs outer: 
#     inner: we can directly compute the variable in period t (default)
#     outer: we need to
#       run the simulation several times to find the variable values
#       that fulfill the condition. outer variables are very computational intensive
#       and will be a special case. We first look at inner variables
#
# level:
#     The computation level. A variable of level k can be computed using only variable
#     from lower levels or a variable of the same cluster from the current level.
#     Usually parameters will have a lower level than variables. 
#
# cluster:
#     One or several variables can form a cluster. 
#     Variables in the same cluster are at the same level.
#     A cluster of one variable is called a singleton.
#
# direct: TRUE or FALSE
#
#      A variable with an explicit formula that forms a singleton cluster.
#      Direct variables can be computed much faster

examples.model.dependencies = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("ThreeEq")
  em = load.model("GreenParadox")
  check.model(em)
  
  init.model(em,solve.systems = TRUE)
  sim = simulate.model(em)
  
  
  Rprof(tmp <- tempfile())
  for (i in 1:3)
    sim = sim = simulate.model(em)
  Rprof()
  summaryRprof(tmp)
  unlink(tmp)

  #cat.sim.fun(em)
  fo.df = em$fo.df
  ifo.df = em$ifo.df
  lifo.df = em$lifo.df
  ofo.df = em$ofo.df
  sim.fun = em$sim.fun
  
  eq_ = ofo.df$eq_[[1]]
  call = quote(
    (all(R >= 0)) & 
    (
      (sum(R) == first(Smax)) |
      (
        (sum(R) <= first(Smax)) & 
        all(m == 0)
      )
    )
  )
  eval(call,sim)  
  
  init.model.scen(em)
  compute.par.mat(em)
  par.mat = em$par.mat

  sim = sim.fun(T = em$T,par.mat = par.mat,var.names = em$var.names)

  Rprof(tmp <- tempfile())
  for (i in 1:500)
    sim = sim.fun(T = em$T,par.mat = as.matrix(par.df),var.names = em$var.names)
  Rprof()
  summaryRprof(tmp)
  unlink(tmp)
  
 
  
  fo.df = em$fo.df
  fo.df = fo.add.dependencies(fo.df)  
  fo.df = fo.solve(fo.df)
  
  em$fo.df
  
  #init.model.scen(em)
  em$init.var
  em$sim = simulate.model(em,T = 200)
  c(sd(sim$p),sd(sim$p_adapt),sd(sim$p_fund))

}

simulate.model = function(em, scen.name = names(em$scenarios)[1], scen = em$scenarios[[scen.name]], init.scen=TRUE, compute.par=TRUE) {
  restore.point("simulate.model")
  if (init.scen)
    init.model.scen(em,scen = scen)
  
  if (compute.par)
    compute.par.mat(em)

  em$var.mat = em$sim.fun(T = em$T,par.mat = em$par.mat,var.names = em$var.names)
  var.mat = em$var.mat
  par.mat = em$par.mat  
  T = em$T

  
  par.df = par.mat[2:(T+1),,drop=FALSE]
  lag.par.df  = par.mat[1:(T),,drop=FALSE]
  lead.par.df = par.mat[3:(T+2),,drop=FALSE]
  
  colnames(lag.par.df) = paste0("lag_", colnames(par.mat))
  colnames(lead.par.df) = paste0("lead_", colnames(par.mat))

  var.mat = em$var.mat
  var.df = var.mat[2:(T+1),,drop=FALSE]
  lag.var.df = var.mat[1:(T),,drop=FALSE]
  colnames(lag.var.df) = paste0("lag_", colnames(var.mat))
  
  sim = as.data.frame(cbind(var.df,par.df,lag.var.df,lag.par.df,lead.par.df))
  sim = add.sim.extra.vars(em,sim)
  em$sim = sim
  invisible(sim)
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
  
  c(li1,li2,li3,li4,li5)
}

make.outer.sim.fun = function(em, inner.sim.fun= em$inner.sim.fun, parent.env=parent.frame()) {
  restore.point("make.outer.sim.fun")
  ofo.df = em$ofo.df
  
  subst.li = fo.code.subst.li(fo.df = ofo.df, var.names = em$var.names, par.names=em$par.names)
  
  # return value of function that nleqslv calls
  impl_ = lapply(ofo.df$impl_, substitute.call, env=subst.li)  
  names(impl_) = NULL
  criterion = as.call(c(list(as.symbol("c")),impl_))

  endo = ofo.df$var
  insert.x = lapply(seq_along(endo), function(i) {
    substitute(init.var.mat[ti,var] <- x[[i]],list(i=i,var=endo[[i]],ti=ofo.df$ti[[i]]))
  }) 
  insert.x = as.call(c(list(as.symbol("{")),insert.x))

  
  # init.x 
  # need better code that takes into account guess
  
  if (FALSE) {
    #restore.point("ndjnfjdngjg")
    rhs = lapply(seq_along(endo), function(i) {
      substitute(var.mat[ti-1,var],list(var=endo[[i]]))
    }) 
    rhs = as.call(c(list(as.symbol("c")),rhs))
    start = substitute(start.x <- rhs, list(rhs=rhs)) 
  }

  init.x = substitute(start.x <- runif(num.endo), list(num.endo=length(endo))) 


    
  fun.body = substitute({
    # init outer variables with their guess value
    init.x
    
    # default set of rows
    ti = (2:(T+1))
    
    # init var.mat matrix if it does not exist
    if (is.null(var.mat)) {
      nvar = length(var.names)
      var.mat = matrix(0,nrow=(T+2),ncol=nvar)
      colnames(var.mat) = var.names
    } 
    
  
    init.var.mat = var.mat
    fn = function(x,T,ti,par.mat, init.var.mat, inner.sim.fun) {
      insert.x
      
      var.mat = inner.sim.fun(T=T,par.mat=par.mat,var.mat=init.var.mat)
      
      vec <- criterion
      sum(vec^2)
    }
    # solve 
    res = optims(par=start.x,fn=fn,T=T,ti=ti,par.mat=par.mat, init.var.mat=init.var.mat,inner.sim.fun=inner.sim.fun)
    if (!res$ok) warning("Outer search did not converge.")
    x = res$par
    
    insert.x
    var.mat = inner.sim.fun(T=T, par.mat=par.mat,var.mat=init.var.mat)
    var.mat
  },list(criterion=criterion, insert.x=insert.x, init.x=init.x)
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
      
      for (ti in fo.start:(T+2)) 
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
          res = mynleqslv(x=start.x,ti=ti,par.mat=par.mat, var.mat=var.mat,
            fn = function(x,ti,par.mat, var.mat) {
            inner
          })$x
          assign
        },
        list(inner=inner, start=start,assign=assign)
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
  if (any(fo.df$outer))
    restore.point("fo.solve.with.outer")

  
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
  cluster.df = as_data_frame(rbindlist(li))
  
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
  em$fo.df = fo.solve(em$fo.df,...)
  em$ifo.df = fo.solve(em$ifo.df,...)
  em$lifo.df = fo.solve(em$lifo.df,...)

}

init.model.vars = function(em) {
  restore.point("init.model.vars")

  # create types
  em$vars = lapply(em$vars, function(var) {
    restore.point("hfhhfuehuh")
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


init.ofo.df = function(em) {
  restore.point("init.outer.vars")
  ofo.df = NULL
  rows = which(is.true(em$ifo.df$outer))
  if (length(rows)>0) {
    iofo = em$ifo.df[rows,]
    iofo$ti = 2
    em$ifo.df = em$ifo.df[-rows,]
  } else {
    iofo = NULL
  }
  rows = which(is.true(em$lifo.df$outer))
  if (length(rows)>0) {
    liofo = em$ifo.df[rows,]
    liofo$ti = 1
    em$lifo.df = em$lifo.df[-rows,]
  } else {
    liofo = NULL
  }
  em$ofo.df = rbind(liofo,iofo)
  em$has.outer = NROW(em$ofo.df) > 0
}


init.vars.formulas = function(em) {
  restore.point("init.vars.formulas")
  # phase t
  li = lapply(names(em$vars), function(var) {
    fo = init.var.formula(em,fo=em$vars[[var]],var=var, phase="t", list.wrap=TRUE)
    fo
  })
  fo.df = as_data_frame(rbindlist(li))
  names(fo.df$eq_) = names(fo.df$expl_) = names(fo.df$impl_) = 
  names(fo.df$dependsOn) = fo.df$var
  em$fo.df = fo.df

  
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
    fo.df = as_data_frame(rbindlist(li))
    names(fo.df$eq_) = names(fo.df$expl_) = names(fo.df$impl_) = 
    names(fo.df$dependsOn) = fo.df$var
    em$ifo.df = fo.df
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
  fo.df = as_data_frame(rbindlist(li))
  names(fo.df$eq_) = names(fo.df$expl_) = names(fo.df$impl_) = 
  names(fo.df$dependsOn) = fo.df$var
  em$lifo.df = fo.df

  init.ofo.df(em)
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
    restore.point("adnjnfjnfhdhfdfbdb")
  }

  if (steady.state) {
    eq_   = substitute.call(eq_,lag.subst.li)
  }
  
  impl_ = adapt.to.implicit.eq(eq_)
    
  #impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(eq_),rhs=get.rhs(eq_)))

  if (list.wrap) {
    res = list(var=var,type=type,expl_=list(expl_),impl_=list(impl_),eq_=list(eq_),outer=as.logical(is.true(fo$outer)))
  } else {
    res = list(var=var,type=type,expl_=expl_,impl_=impl_,eq_=eq_,outer=as.logical(is.true(fo$outer)))
  }
  
  dep.li = compute.var.dependsOn(em,var, impl_, list.wrap=list.wrap)
  
  c(res,dep.li)
}


get.var.formula.type = function(fo) {
  restore.point("get.var.formula.type")
  types = c("xcurve","ycurve","formula","xcut","ycut","eq")
  not.null = sapply(types, function(type) {!is.null(fo[[type]])})
  types[not.null]
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
  all = unique(c(unlist(em$ofo.df$dependsOn),
                 unlist(em$fo.df$dependsOn),
                 unlist(em$ifo.df$dependsOn),
                 unlist(em$lifo.df$dependsOn)))
  pars = setdiff(all,c(em$var.names,
    paste0("lag_",em$var.names),paste0("lead_",em$var.names)))
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


# faster version with fewer checks
mynleqslv = function (x, fn, jac = NULL, ..., method = c("Broyden", "Newton")[1], 
    global = c("dbldog", "pwldog", "cline", "qline", "gline", 
        "hook", "none")[1], xscalm = c("fixed", "auto")[1], jacobian = FALSE) 
{
    #restore.point("mynleqslv")
    fn1 <- function(par) fn(par, ...)
    jac1 <- if (!is.null(jac)) 
        function(par) jac(par, ...)
    con <- list(ftol = 1e-08, xtol = 1e-08, btol = 0.001, stepmax = -1, 
        delta = -2, sigma = 0.5, scalex = rep(1, length(x)), 
        maxit = 150, trace = 0, chkjac = FALSE, cndtol = 1e-12, 
        allowSingular = FALSE, dsub = -1L, dsuper = -1L)

    on.exit(.C("deactivatenleq", PACKAGE = "nleqslv"))
    out <- .Call("nleqslv", x, fn1, jac1, method, global, xscalm, 
        jacobian, con, new.env(), PACKAGE = "nleqslv")
    out
}

cat.sim.fun = function(em) {
  if (!is.null(em$inner.sim.fun)) {
    cat("\n",
      '\n#restore.point("inner.sim.fun")',
      '\ninner.sim.fun <-',
      deparse1(em$inner.sim.fun,collapse = "\n"),
      "\n\n"
    )
  }
  cat("\n",
    '\n#restore.point("sim.fun")',
    '\nsim.fun <-',
    deparse1(em$sim.fun,collapse = "\n"),
    "\n\n"
  )
  
}

# not yet implemented
make.model.jacobi = function(em) {
  ex = do.call(expression, em$impl)
  lapply(em$impl,function(call) {
    deriv(call,vars)
  })

}

check.model = function(em) {
  restore.point("check.model")
  
  # all known symbols
  syms = c(names(em$vars),names(em$params), names(em$extraVars))
  li = lapply(em$scenarios, function(scen) names(scen$params))
  syms = unique(c(syms, unlist(li)))
  syms = c(syms, paste0("lag_",syms),paste0("lead_",syms),"t")
  
  check.formula = function(obj,field="formula",name=get.name(obj),section="") {
    if (length(field)>1) {
      code = paste0("obj", paste0("[['",field,"']]",collapse=""))
      val = eval(parse(text=code))
    } else {
      val = obj[[field]]
    }
    
    if (length(val)==0) return(TRUE)
    vars = find.variables(parse.as.call(val))
    unknown = setdiff(vars,syms)
    if (length(unknown)>0) {
      str = paste0("\n\nReference to unknown symbol ", paste0(unknown,collapse=", "), " in ", section, " -> ", name, " -> ",field)
      cat(str)
      return(FALSE)
    }
  }
  
  # check variables
  lapply(em$vars, check.formula, section="vars")
  lapply(em$vars, check.formula, section="vars", field=c("init","formula"))
  lapply(em$vars, check.formula, section="vars", field=c("init","eq"))
  lapply(em$vars, check.formula, section="vars", field=c("laginit","formula"))
  lapply(em$extraVars, check.formula, section="extraVars")
  lapply(em$panes, check.formula, section="panes", field="xmarker")
  lapply(em$panes, check.formula, section="panes", field="ymarker")

  invisible(TRUE)
  
}

