
examples.eqs.solution.report = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("Capital3Eq")
  em = load.model("General3Eq")
  check.model(em)
  
  init.model(em,solve.systems = !TRUE)
  init.model.scen(em = em)
  res = solve.steady.state(em)
  clu.df = res$clu.df
  exo = res$exo
  sim = simulate.model(em)
  

}


eqs.solution.report = function(eqs, var.li=NULL, exo=NULL, verbose=TRUE, funs=NULL) {
  restore.point("eqs.solution.report")
  
  txt = NULL
  w = function(...) txt <<- c(txt,...)
  write.cluster.df = function(df) {
    levels = unique(df$level)
    for (lev in levels) {
      clusters = unique(df$cluster[df$level==lev])
      w(paste0("Level ", lev))
      for (cluster in clusters) {
        rows = which(df$cluster==cluster)
        w(paste0("  Cluster ", cluster, " (",length(rows),")"))
        str = paste0("    ",rows,". ",df$var[rows],"=",signif(df$val[rows],3), " : ", sapply(df$eq_[rows],deparse1))
        if (any(!is.finite(df$val[rows])) & min(rows)>1) {
          brows = 1:(min(rows)-1)
          vals = c(exo,df$val[brows] )
          subst = lapply(vals, signif, digits=4)
          names(subst) = c(names(exo),df$var[brows])
          num.eqs = lapply(df$eq_[rows], substitute.call, env=subst)
          str = paste0(str, "  <=>  ", num.eqs)
        }
        w(str)
      }
    }
  }

  
  
  w("# Part 1: Original equations\n", 
    paste0("  ",seq_along(eqs),". ", sapply(eqs,deparse1))
  )

  # Solve functions and derivatives
  eqs = compute.equation.funs(eqs,funs)
  # Transform into clusters
  df = cluster.equations(eqs = eqs, var.li=var.li, exo=names(exo), solve.symbolic=FALSE, skip.eat=TRUE, skip.big=TRUE)
  
  df$val = eval.cluster.df(clu.df = df, exo=exo)

  w("\n# Part 2: Substitute functions and derivatives, put in one big cluster and try to solve\n", 
    paste0("  ",seq_along(eqs),". ", sapply(eqs,deparse1))
  )
  write.cluster.df(df)

  w("
\n# Part 3: Eat away equations from big cluster

            from front: equations with single cluster variables
            from back:  cluster variables contained in a single equation\n")
  
  test.df = df; 
  cluster = test.df$cluster[which.max(test.df$cluster.size)]
  test.df = eat.from.cluster(df=test.df,cluster=cluster, repeated=FALSE,eating.funs = list(eat.single.front))
  any(duplicated(test.df$var))
  
  cluster = test.df$cluster[which.max(test.df$cluster.size)]
  test.df = eat.from.cluster(df=test.df,cluster=cluster, repeated=FALSE,eating.funs = list(eat.single.back))
  any(duplicated(test.df$var))

  
    
  df = eat.from.cluster(df=df, cluster=1)
  df$val = eval.cluster.df(clu.df = df, exo=exo)

  write.cluster.df(df)

  #cluster.subst.var(df = df, cluster=cluster, var = "Wdem",eq.ind=7)
  w("
\n# Part 4: Try to solve single equations and systems (by substitution method) symbollically\n
    Note: The algorithm does not automatically detect multicolinarity, i.e. underdeterminiation
    of the equation system. 
    If the results seem unreasonable, try adding conditions or fix values.\n\n")
  
  df = solve.symbolic.cluster.df(df)
  df$val = eval.cluster.df(clu.df = df, exo=exo)
  write.cluster.df(df)
   
  cat(paste0(txt,collapse="\n"))
  
  res = symbolic.cluster.subst.eqs(df=df, cluster=7)
  ndf = res$df
  
  rows = which(df$cluster==7)
  eqs = df$eq_[rows]
  vars = df$var[rows]
  symbolic.subst.eqs(eqs=eqs, vars=vars)
  
  
  df = df
  vals = df$val
  names(vals) = df$var
  exo.vals = c(exo, vals)
}



eval.cluster.df = function(clu.df, exo = list()) {
  restore.point("eval.cluster.df")
  
  exo = lapply(exo, as.numeric)
  vals = rep(NA_real_,NROW(clu.df))
  names(vals)  = clu.df$var
  clusters = sort(unique(clu.df$cluster))
  
  cluster = 1
  for (cluster in clusters) {
    fun = make.cluster.solve.fun(cluster=cluster, clu.df=clu.df)
    sol = try(fun(vals=c(exo,vals)))
    if (is(sol,"try-error")) break
    if (!is.list(sol)) {
      vals[[names(sol)]] = sol
    } else {
      if (!sol$ok) break
      cat("\nsolved cluster ", cluster)
      vals[names(sol$par)] = sol$par
    }
  }
  vals  
}

numerical.solve.eqs = function(eqs,vars, exo) {
  solve.fun = numerical.solve.eqs.fun(eqs,vars)
  solve.fun(exo)
}

numerical.solve.eqs.fun = function(eqs, vars) {
  restore.point("eqs.numerical.solve.fun")
  
  syms = unique(unlist(lapply(eqs,find.variables)))
    
  xsubst = lapply(seq_along(vars), function(i) {
    substitute(x[i],list(i=i))
  })
  names(xsubst) = vars
    
  exo = setdiff(syms, vars)
    
  vsubst = lapply(exo, function(par) {
    substitute(vals[[par]],list(par=par))
  })
  names(vsubst)=exo
    
  # Create function
  impl_ = lapply(eqs, function(eq_) {
    call = substitute(lhs-(rhs), list(lhs=eq_[[2]],rhs=eq_[[3]]))
    call = substitute.call(call, c(xsubst,vsubst))
    call
  })
  names(impl_) = NULL
  inner = as.call(c(list(as.symbol("c")),impl_))
  
  fn = function(x,vals) {}
  body(fn) = substitute({inner}, list(inner=inner))
  fn
  
  #code = deparse1(body(fn))
  #code = sep.lines(code,",")
  #code

  var.assign.li = lapply(seq_along(vars), function(i) {
    substitute(var <- x[i], list(var=as.name(vars[i]),i=i))
  })
  var.assign = as.call(c(list(as.symbol("{")),var.assign.li))

  code = substitute({
    x <- runif(len.vars) 
    fn <- fun  
    #sol = mynleqslv(x=x, fn=fn)     
    sol = optims(par=x, eq.fn=fn, vals=vals)     
    if (!sol$ok) {
      warning(paste0("Could not solve ", paste0(endo, collapse=", "),":\n", sol$message ))
      sol$par[] = NA_real_
    }
    names(sol$par) = endo
    sol
  }, list(fun=fn,len.vars=length(vars),endo=vars, var.assign=var.assign))
  
  solve.fun = function(vals) {}
  body(solve.fun) = code
  solve.fun
}  
  
make.cluster.solve.fun = function(cluster=clu.df$cluster[[1]], clu.df) {
  restore.point("make.cluster.solve.code")
  
  df = clu.df[clu.df$cluster==cluster,]
  
  vars = df$var
  solved = df$solved[1]
  if (solved & NROW(df)>1)
    stop("We have a solved cluster with more than 1 row")
    
  if (solved) {
    expl = df$expl_[[1]]
    syms = find.variables(expl)
    vsubst = lapply(syms, function(par) {
      substitute(vals[[par]],list(par=par))
    })
    names(vsubst)=syms
    expl = substitute.call(expl, vsubst)
    code = substitute({val <- rhs; names(val)=var; val}, list(var=df$var, rhs = expl))
    solve.fun = function(vals) {}
    body(solve.fun) <- code
  } else {
    solve.fun = numerical.solve.eqs.fun(eqs=df$eq_,vars)
  }

  solve.fun
  
}


cluster.equations = function(eqs, var.li=NULL, endo=NULL, exo=NULL, verbose=TRUE, solve.symbolic=TRUE, skip.eat=FALSE, solve.level = 100, skip.big=FALSE, funs=NULL, cluster.df=NULL) {
  restore.point("cluster.equations")


  eqs = compute.equation.funs(eqs,funs)
  # Find for each formula the contained endogenous variables
  if (is.null(var.li)) {
    var.li = lapply(eqs, function(form) {
      vars = find.variables(form)
      vars = setdiff(vars, exo)
      if (!is.null(endo)) vars = intersect(vars, endo)
      vars
    }) 
  }
  syms = unique(unlist(var.li))

  # deal with free symbols (more variables than equations)
  # add dummy equations
  num.free = length(syms)-length(eqs)
  if (num.free>0) {
    if (verbose)
      cat("\nWe have", num.free, "more variables than equations. Include dummy equations 0==0 to match number of equations with number of variables.")
    free.rows = (NROW(df)+1):(NROW(df)+num.free)
    eqs = c(eqs, replicate(n=num.free, quote(0==0)))
    var.li = c(var.li, vector("list",num.free))  
  } else if (num.free < 0) {
    if (verbose)
      cat("\nWe have", -num.free, "more equations than variables. Include dummy variables DUMMY_1_,...")
    syms = c(syms, paste0("DUMMY___", 1:(-num.free)))

  }
  nr = length(eqs)
  vars.id = sapply(var.li, function(vars) {
    if (length(vars)==0) return("")
    paste0(sort(vars), collapse="|")
  })
    
  df = data_frame(var=syms,expl_ = vector("list",nr),eq_ = eqs,org_=eqs, solved=FALSE, level=1,cluster=1,cluster.size=length(eqs), vars=var.li, vars.id = vars.id, num.vars=sapply(var.li, length),org.ind=seq_along(eqs), val=NA_real_)
  
  
  if (!skip.eat) {
    df = eat.from.cluster(df, cluster=1)
  }
  if (solve.symbolic)  {
    df = solve.symbolic.cluster.df(df, skip.big=skip.big)
  }
  df
}

solve.symbolic.cluster.df = function(df, skip.big=FALSE, skip.small=FALSE, eq.solve.fun=sym.solve.eq,  simplify.fun = Deriv::Simplify) {
  restore.point("solve.symbolic.cluster.df")

  if (!skip.small) {
    rows = which(df$cluster.size == 1 & !df$solved)
    row = rows[1]
    for (row in rows) {
      res = eq.solve.fun(df$eq_[[row]], df$var[row])
      if (!res$solved) next
      expl = simplify.fun(res$eq[[3]])
      df$eq_[[row]] = substitute(lhs==rhs,list(lhs=as.name(df$var[row]), rhs=expl))
      df$expl_[[row]] = expl
      df$solved[row] = TRUE
    }
  }
  
  if (!skip.big) {
    clusters = unique(df$cluster[df$cluster.size>1])
    
    for (cluster in clusters) {  
      res = symbolic.cluster.subst.eqs(df = df, cluster=cluster,rename.clusters = FALSE)
      df = res$df
    }
    if (length(clusters)>0) {
      # now reorder clusters and levels
      df$cluster = rank(df$cluster,ties.method = "min")
      df$level = rank(df$level,ties.method = "min")
      df = df[order(df$cluster),,drop=FALSE]
    }
  }
  df
}

eat.from.cluster = function(df, cluster=df$cluster[1], eating.funs = list(eat.single.front,eat.single.back,eat.double.front), mat=NULL, repeated=TRUE) {
  restore.point("eat.from.cluster")
  
  num.funs = length(eating.funs)
  fun.ind = last.fun = 1

  while(TRUE) {
    fun = eating.funs[[fun.ind]]
    while (TRUE) {
      res = fun(df=df,cluster=cluster, mat=mat)
      if (res$changed==0) {
        if (!repeated) last.fun = fun.ind
        break
      }
      df = res$df; mat = res$remaining.mat; cluster = res$remaining.cluster
      last.fun = fun.ind
      if (!repeated) break
    }
    fun.ind = ((fun.ind +1 -1) %% num.funs)+1
    if (fun.ind ==last.fun) break
  }
  df
}

old.eat.from.cluster.df = function(df, cluster=df$cluster[1], var.li = df$vars, syms=df$var, skip.eat = FALSE, only.calls=NULL, repeated = TRUE) {
  
  nr = length(var.li); nc = length(syms)
  mat = matrix(0,nr,nc)
  colnames(mat) = syms
  for (i in seq_along(var.li)) {
    mat[i,var.li[[i]]] = 1
  }
  mat
  
  if (!skip.eat) {
    calls = create.eating.calls()
    if (!is.null(only.calls)) {
      calls = calls[only.calls]
    }
    sum.changed = 1
    while(sum.changed>0) {
      sum.changed = 0
      changed = 0
      for (call in calls) {
        eval(call)
        sum.changed = sum.changed + changed
      }
      if (!repeated) break
    }
  }
    
  # put remaining rows into a cluster
  rows = which(df$cluster==0)
  if (length(rows)>0) {
    remain.syms = setdiff(syms, df$var)
    
    df$cluster[rows] = max(df$cluster)+1
    df$cluster.size[rows] = length(rows)
    df$var[rows] = remain.syms    
    df$level[rows] = max(df$level)+1
  }  

  # reindex negative clusters and levels 
  # put -1 at end, before it -2 and so on...
  num.cluster = length(unique(df$cluster))
  num.level = length(unique(df$level))
  
  rows = which(df$cluster < 0) 
  new.cluster = num.cluster + df$cluster[rows] +1
  new.level = num.level + df$level[rows] +1
  df$cluster[rows] = new.cluster
  df$level[rows] = new.level    
  df = df[order(df$cluster),]  
  df
}

symbolic.cluster.subst.eqs = function(df, cluster, rename.clusters=TRUE) {
  restore.point("cluster.subst.var")
  
  rows = which(df$cluster==cluster)
  res = symbolic.subst.eqs(eqs=df$eq_[rows], vars=df$var[rows])
  
  erows = rows[res$eq.inds]
  srows = rows[res$subst.inds]
  ns = length(srows)

  # No equation could be substituted
  if (ns==0)
    return(list(df=df,remaining.cluster=cluster, new.clusters=NULL))

  
  if (length(res$eqs)>0) {
    df$var[erows] = res$vars
    df$cluster.size[erows] = length(erows)
    df$eq_[erows] = res$eqs
  }
  
  df$var[srows] = res$subst.vars
  df$eq_[srows] = res$subst.eq
  df$expl_[srows] = lapply(res$subst.eq, function(eq) eq[[3]])
  df$solved[srows] = TRUE
  df$cluster.size[srows] = 1
  df$cluster[srows] = cluster + (1:ns) / (ns+1)
  df$level[srows] = df$level[rows[1]] + (1:ns) / (ns+1)

  if (rename.clusters) {
    df$cluster = rank(df$cluster,ties.method = "min")
    df$level = rank(df$level,ties.method = "min")
  }
  
  remaining.cluster = df$cluster[erows[1]]
  new.clusters = df$cluster[srows]
  
  df = df[order(df$cluster),]
  
  return(list(df=df,remaining.cluster=remaining.cluster, new.clusters=new.clusters))

}

symbolic.cluster.subst.var = function(df, cluster, var, eq.ind=1) {
  restore.point("cluster.subst.var")
  
  rows = which(df$cluster==cluster)
  res = symbolic.subst.var(df$eq_[rows], var=var, eq.ind=eq.ind)
  if (!res$ok) return(list(ok=FALSE,df=df,remaining.cluster=cluster, new.cluster=NA))

  erow = rows[eq.ind]
  df$eq_[[erow]] = res$subst.eq
  if (length(rows)==1)
    return(list(ok=TRUE,df=df,remaining.cluster=NA, new.cluster=cluster))

  syms = df$var[rows]
  rsyms = setdiff(syms,var)
  rrows = setdiff(rows,erow)
  df$cluster[erow] = df$cluster[erow]+0.5
  df$level[erow] = df$level[erow]+0.5
  
  df$cluster = rank(df$cluster,ties.method = "min")
  df$level = rank(df$level,ties.method = "min")
  df$cluster.size[rrows] = df$cluster.size[rrows[1]]-1
  df$cluster.size[erow] = 1
  df$var[rrows] = rsyms
  df$var[erow] = var
  
  remaining.cluster = df$cluster[rrows[1]]
  new.cluster = df$cluster[erow]
  
  df = df[order(df$cluster),]
  
  return(list(ok=TRUE,df=df,remaining.cluster=remaining.cluster, new.cluster=new.cluster))
  
}

symbolic.subst.var = function(eqs, var, eq.ind=1, eq.solve.fun=sym.solve.eq,  simplify.fun = Deriv::Simplify, expl=NULL) {
  restore.point("symbolic.subst.var")

  eq = eqs[[eq.ind]]
  if (is.null(expl)) {
    res = eq.solve.fun(eq,var)
    if (!res$solved) {
      return(list(ok=FALSE, eqs=eqs, subst.eq=NULL))
    }
    subst.eq = res$eq
    expl_ = subst.eq[[3]]
  } else {
    subst.eq = substitute(lhs == rhs, list(lhs=as.name(var), rhs=expl))
    expl_ = expl
  }
  
  # Try to simplify the explicit solution
  if (!is.null(simplify.fun)) {
    expl_ = simplify.fun(expl_)
    subst.eq = substitute(lhs==rhs, list(lhs=as.name(var),rhs=expl_))
  }  
  reqs = eqs[-eq.ind]
  # substitute solution into remaining equations
  subst.li = list(substitute((expl_),list(expl_=expl_)))
  names(subst.li) = var
  reqs = lapply(reqs, substitute.call,  env=subst.li)
  if (!is.null(simplify.fun)) {
    reqs = lapply(reqs, simplify.fun)
  }
  return(list(ok=TRUE, eqs=reqs, subst.eq=subst.eq))
}

symbolic.subst.eqs = function(eqs,vars, subst.eqs=NULL, subst.vars=NULL, eq.inds=seq_along(eqs), subst.inds=NULL) {
  
  restore.point("symbolic.subst.eqs")
  if (length(eqs)==0) {
    return(list(eqs=eqs, vars=vars, subst.eqs=subst.eqs, subst.vars=subst.vars, eq.inds=eq.inds, subst.inds=subst.inds))
  }
  sug = suggest.subst.eq.and.var(eqs, vars)
  if (!sug$ok) {
    return(list(eqs=eqs, vars=vars, subst.eqs=subst.eqs, subst.vars=subst.vars, eq.inds=eq.inds, subst.inds=subst.inds))
  }
  var = sug$var; eq.ind = sug$eq.ind
  res = symbolic.subst.var(eqs = eqs,var = var,eq.ind = eq.ind,expl=sug$expl)
  
  # Call again with remaining values
  symbolic.subst.eqs(eqs = res$eqs, vars = setdiff(vars,var), subst.eqs=c(res$subst.eq,subst.eqs), subst.vars = c(var, subst.vars), eq.inds=setdiff(eq.inds,eq.inds[eq.ind]), subst.inds=c(eq.inds[eq.ind], subst.inds)) 
}

# Heuristics that suggest the next variable and equation to be substituted 
# when solving a system of equations by substitution method
suggest.subst.eq.and.var = function(eqs,vars,eq.solve.fun=sym.solve.eq) {
  restore.point("suggest.subst.eq.and.var")
  
  lhs = lapply(eqs, function(eq) eq[[2]])
  rhs = lapply(eqs, function(eq) eq[[3]])
  lhs.single = sapply(lhs, is.name)
  rhs.single = sapply(rhs, is.name)

  lhs.var = sapply(seq_along(lhs), function(ind) {
    if (!lhs.single[ind]) return("")
    as.character(lhs[[ind]])
  })
  rhs.var = sapply(seq_along(rhs), function(ind) {
    if (!rhs.single[ind]) return("")
    as.character(rhs[[ind]])
  })
  
  lhs.single = lhs.single & lhs.var %in% vars
  rhs.single = rhs.single & rhs.var %in% vars
  

  
  identity = which(lhs.single & rhs.single)
  
  # If we have identities, propose them
  if (length(identity)>0) {
    row = identity[1]
    # substitute the longer variable
    lvar = as.character(lhs[[row]])
    rvar = as.character(rhs[[row]])
    if (nchar(lvar)>nchar(rvar) | (! rvar %in% vars) ) {
      return(list(ok=TRUE,eq.ind=row, var=lvar, expl=rhs[[row]], type="id"))
    } else {
      return(list(ok=TRUE,eq.ind=row, var=rvar, expl=lhs[[row]], type="id"))
    }
  }
  
  # check for explicit solutions
  var = sapply(c(lhs[lhs.single], rhs[rhs.single]), as.character)
  expl = c(rhs[lhs.single], lhs[rhs.single])
  is.expl = sapply(seq_along(expl), function(i) ! (var[i] %in% intersect(vars,find.variables(expl[[i]]) )))
  if (any(is.expl)) {
    var = var[is.expl]
    expl = expl[is.expl]
    eq.ind = c(which(lhs.single),which(rhs.single))[is.expl]
    # Take shortest expl but prefer longer variables names
    size = sapply(expl, call.size) - (nchar(var) / 100)
    row = which.min(size) 
    return(list(ok=TRUE,eq.ind=eq.ind[row], var=var[row], expl=expl[[row]], type="expl"))
  }
  
  # go through all equations by size
  size = sapply(eqs, call.size)
  eq.inds = order(size)
  eq.ind = eq.inds[5]
  for (eq.ind in eq.inds) {
    eq = eqs[[eq.ind]]
    vt = count.variables(eq)
    evars = intersect(names(vt), vars)
    vt = vt[evars]
    ord = order(vt - nchar(evars) / 100)
    for (var in evars[ord]) {
      res = eq.solve.fun(eq,var)
      if (res$solved) {
        return(list(ok=TRUE,eq.ind=eq.ind, var=var, expl=res$eq[[3]], type="solved"))
      }
    }
  }
    
  # Could not solve any variable  
  return(list(ok=FALSE,eq.ind=NA, var="", expl=NULL, type="unsolved"))
}

symbolic.cluster.equations = function(eqs, exo=NULL, endo=NULL, eq.solve.fun=sym.solve.eq, level=-1, subst=NULL, skip.big=FALSE, simplify.fun = Deriv::Simplify) {
  restore.point("symbolic.cluster.equations")
  
  if (length(eqs)==2 & "K" %in% endo) restore.point("nfjdmfgdkgrngdgfgfhzbgvf")
  vars = unique(unlist(lapply(eqs, find.variables)))
  if (is.null(endo)) {
    endo = setdiff(vars, exo)
  } else {
    free.var = setdiff(endo,vars)
    if (length(free.var)>0) {
      warning("The variable(s) ", paste0(free.var)," cannot be determined. Please specify an initial value for them.")
    }
  } 

  if (length(endo) != length(eqs)) {
    stop("number of endo variables and number of equations is not identical")
  }
  
  # check if one equation is already in form of an explicit solution
  solved = FALSE
  if (! (skip.big & length(eqs)>1)) {
    for (eq.ind in seq_along(eqs)) {
      var = is.expl.eq(eqs[[eq.ind]],endo)
      if (!is.null(var)) {
        new.eq = eqs[[eq.ind]]
        solved = TRUE
        break
      }
    }
    # try to solve an equation for an endogenous variable
    # using the symbolic equation solver
    if (!solved) {
      sym.li = lapply(eqs, function(eq) find.variables(eq))
      for (eq.ind in seq_along(eqs)) {
        eq = eqs[[eq.ind]]
        vars = intersect(sym.li[[eq.ind]],endo)
        for (var in vars) {
          res = eq.solve.fun(eq,var)
          if (res$solved) {
            solved = TRUE
            new.eq = res$eq
            break
          }        
        }
        if (solved) break
      }
    }
  }  
  # could solve an equation
  if (solved) {
    expl_ = new.eq[[3]]
    
    # Try to simplify the explicit solution
    if (!is.null(simplify.fun))
      expl_ = simplify.fun(expl_)
    
    # try to solve remaining equations,
    # plugging in the solved value
    if (length(eqs)>1) {
      #restore.point("symbolic.cluster.equations.outer")
      reqs = eqs[-eq.ind]
      child.inds = seq_along(eqs)[-eq.ind]
      

      # substitute solution into remaining equations
      subst.li = list(substitute((expl_),list(expl_=expl_)))
      names(subst.li) = var
      reqs = lapply(reqs, substitute.call,  env=subst.li)
      
      rdf = symbolic.cluster.equations(reqs, endo=setdiff(endo,var),eq.solve.fun = eq.solve.fun,level=level-1, subst=c(subst, var))
      rdf$org.ind = child.inds[rdf$org.ind]
    } else {
      rdf = NULL
    }
    restore.point("nhfnfngigtirmgjnng")
    
    eq.df = data_frame(var=var,expl_=list(expl_), eq_ = list(new.eq), cluster.size = 1, solved=TRUE, level=level, org.ind=eq.ind, subst=list(subst))
    df = rbind(rdf, eq.df)
  } else {
    if (!is.null(simplify.fun))
      eqs = lapply(eqs, simplify.fun)
    
    df = data_frame(var=endo,expl_=vector("list",length(endo)), eq_ = eqs, cluster.size = length(endo), solved=FALSE, level=level, org.ind = seq_along(eqs), subst=replicate(length(endo),subst,simplify = FALSE) )
  } 
  df
}

# is an equation already in explicit form with the variable on the lhs?
is.expl.eq = function(eq, vars=NULL) {
  restore.point("is.expl.eq")
  lhs = eq[[2]]
  if (!is.name(lhs)) return(NULL)

  lhs.var = as.character(lhs)
  # a different lhs variable than the endogenous variables
  if (!is.null(vars)) {
    if (!lhs.var %in% vars) return(NULL)
  }
  # lhs variable is also on rhs
  if (lhs.var %in% find.variables(eq[[3]])) return(NULL)
  
  # indeed in explicit form
  lhs.var
}

make.eating.matrix = function(df, var.li=df$vars, syms=df$var) {
  nr = NROW(df); nc=NROW(df)
  mat = matrix(0,nr,nc)
  colnames(mat) = df$var
  for (i in seq_along(var.li)) {
    mat[i,intersect(var.li[[i]], syms)] = 1
  }
  mat
}

# search for equations that have a single variable
# remove them from the cluster and put them in front
eat.single.front = function(df, cluster, mat=NULL, repeated=FALSE) {
  restore.point("eat.single.front")
  
  crows = which(df$cluster==cluster)
  if (is.null(mat)) {
    mat = make.eating.matrix(df)
    mat = mat[crows,crows]
  }
  syms = colnames(mat)
  # equations that have a single variable
  rows = which(rowSums(mat) == 1)
  if (length(rows)==0)
    return(list(changed=0, df=df, remaining.cluster=cluster, extracted.clusters=NULL, remaining.mat=mat))
  
  # variables that correspond to those equations
  cols = max.col(mat[rows,,drop=FALSE])
        
  esyms = syms[cols]
  if (any(duplicated(esyms))) {
    dupl.syms = unique(esyms[duplicated(esyms)])
    
    txt = paste0("The variables ", paste0(dupl.syms, collapse=", ")," are determined by more than one equation:")
    
    for (sym in dupl.syms) {
      inds = which(esyms==sym)
      arows = crows[rows[inds]]
      aeqs = df$eq_[arows] 
      txt = c(txt, paste0("  ",sym,":"), paste0("    - ",sapply(aeqs, deparse1),collapse="\n"))
    }
    txt = paste0(txt, collapse="\n")
    warning(txt)
  }
  
  erows = crows[rows]
  rrows = setdiff(crows, erows)
  rsyms = setdiff(syms, esyms)
  ne = length(rows)
  remaining.mat=mat[-rows,rsyms, drop=FALSE]
    
  df$cluster[erows] = cluster - (1:ne) / (ne+1)
  df$level[erows] = df$level[erows[1]] - 0.5
  df$cluster = rank(df$cluster,ties.method = "min")
  df$level = rank(df$level,ties.method = "min")
  df$cluster.size[erows] = 1
  df$cluster.size[rrows] = df$cluster.size[rrows[1]]-ne
  df$var[erows] = esyms
  df$var[rrows] = rsyms
  
  remaining.cluster = df$cluster[rrows[1]]
  extracted.clusters = unique(df$cluster[erows])
  
  df = df[order(df$cluster),]
  changed = length(rows)
        
  if (verbose) {
    cat("\neat.single.front:")
    cat("\nvars: ", paste0(esyms, collapse=", "))
    cat("\nrows: ", paste0(erows, collapse=", "),"\n")
  }
  
  if (repeated) {
    res = eat.single.front(df=df, cluster = remaining.cluster, mat=remaining.mat)
    remaining.cluster = res$remaining.cluster
    remaining.mat = res$remaining.mat
  }

  return(list(changed=changed, df=df, remaining.cluster=remaining.cluster, extracted.clusters=extracted.clusters, remaining.mat=remaining.mat))

  
}


eat.double.front = function(df, cluster, mat=NULL) {
  restore.point("eat.double.front")
  
  crows = which(df$cluster==cluster)
  if (is.null(mat)) {
    mat = make.eating.matrix(df[crows,])
  }
  syms = colnames(mat)

  # find equations that contain the same two variables
  vars.id = paste0(as.data.frame(t(mat)), sep="|")

  rows = which((duplicated(vars.id) | duplicated(vars.id, fromLast = TRUE)) & rowSums(mat) == 2)
  
  if (length(rows)==0)
    return(list(changed=0, df=df, remaining.cluster=cluster, extracted.clusters=NULL, remaining.mat=mat))

  changed = length(rows)
  first = which(duplicated(vars.id, fromLast = TRUE))
  
  erows = crows[rows]  
  df$cluster.size[erows] = 2
  df$level[erows] = df$level[crows[1]]-0.5
  
  counter = 0      
  esyms = NULL
  for (row in first) {
    counter = counter+1
    srows = crows[rows[vars.id[rows]==vars.id[row]]]
    cur.syms = syms[which(mat[row,]==1)]
    esyms = c(esyms, cur.syms)
    df$cluster[srows] = cluster+ counter / (length(rows)+1)
    df$var[srows] = cur.syms
  }

  if (verbose) {
    cat("\neat.double.front: ")
    cat("\nvars: ", paste0(esyms, collapse=", "))
    cat("\nrows: ", paste0(erows, collapse=", "),"\n")
  }

  rrows = setdiff(crows, erows)
  rsyms = setdiff(syms, esyms)
  ne = length(rows)
  remaining.mat=mat[-rows,rsyms]
    
  df$level[erows] = df$level[erows[1]] - 0.5
  df$cluster = rank(df$cluster,ties.method = "min")
  df$level = rank(df$level,ties.method = "min")
  df$cluster.size[rrows] = df$cluster.size[rrows[1]]-ne
  df$var[rrows] = rsyms
  
  remaining.cluster = df$cluster[rrows[1]]
  extracted.clusters = unique(df$cluster[erows])
  
  df = df[order(df$cluster),]
  changed = length(rows)
   
  return(list(changed=changed, df=df, remaining.cluster=remaining.cluster, extracted.clusters=extracted.clusters, remaining.mat=remaining.mat))

  
}


# Try to remove equations at the back of a cluster
# search for variables that appear in only one equation
# this equation must then define this variable
# we can put the equation behind the larger cluster
eat.single.back = function(df, cluster, mat=NULL, repeated=FALSE) {
  restore.point("eat.single.back")
  
  crows = which(df$cluster==cluster)
  if (is.null(mat)) {
    mat = make.eating.matrix(df[crows,])
  }
  syms = colnames(mat)

  # Compute for each symbol the number of equations 
  # it is still contained in
  num.eqs = colSums(mat)
        
  # Find the symbol columns that are only in a single
  # equation
  cols = which(num.eqs==1)
  if (length(cols)==0) {
    return(list(changed=0, df=df, remaining.cluster=cluster, extracted.clusters=NULL, remaining.mat=mat))
  }
  # Identify the equations that have the single columns
  rows = which(rowSums(mat[,cols, drop=FALSE]) >= 1)
  
  
  # Error two or more variables are uniquely defined by the same equation:
  # we cannot solve that equation
  if (length(rows)<length(cols)) {
    cols.rows = sapply(cols, function(col) which(mat[,col]==1))
    dup.rows = unique(cols.rows[duplicated(cols.rows) | duplicated(cols.rows,fromLast = TRUE)])
    
    txt = "Equation error found in eat.single back."
    for (dup.row in dup.rows) {
      dup.vars = names(cols.rows)[cols.rows==dup.row]
      dup.eq = crows[dup.row]
      txt = c(txt, paste0("  - The variables ", paste0(dup.vars,collapse=", "), " are only defined in the single equation ", dup.eq, ":\n    ",deparse1(df$eq_[[dup.eq]])))
      if (!identical(df$eq_[[dup.eq]],df$org_[[dup.eq]])) {
        txt = c(txt, paste0("  Original form was:\n    ",deparse1(df$org_[[dup.eq]]) ))
      }
    }
    warning(paste0(txt,collapse="\n"))
    return(list(changed=0, df=df, remaining.cluster=cluster, extracted.clusters=NULL, remaining.mat=mat))
    
  }
  
  ne = length(rows)
  erows = crows[rows]
  later.level = df$level[crows[1]]+0.5
  if (verbose) {
    cat("\neat.single.back: ")
    cat("\nvars: ", paste0(names(cols), collapse=", "))
    cat("\nrows: ", paste0(erows, collapse=", "),"\n")
  }
  df$level[erows] = later.level
  df$cluster[erows] = cluster + (1:ne) / (ne+1)
  df$cluster.size[erows] = 1
  for (col in cols) {
    crow = crows[which(mat[,col] >= 1)]
    df$var[crow] = syms[col]
  }
  
  remaining.mat = mat[-rows, -cols]
  
  esyms = syms[cols]
  rrows = setdiff(crows, erows)
  rsyms = setdiff(syms, esyms)
  ne = length(rows)

  df$cluster = rank(df$cluster,ties.method = "min")
  df$level = rank(df$level,ties.method = "min")
  df$cluster.size[rrows] = df$cluster.size[rrows[1]]-ne
  df$var[rrows] = rsyms
  
  remaining.cluster = df$cluster[rrows[1]]
  extracted.clusters = unique(df$cluster[erows])
  
  df = df[order(df$cluster),]
  changed = ne

  if (repeated) {
    res = slow.eat.single.back(df=df, cluster = remaining.cluster, mat=remaining.mat)
    remaining.cluster = res$remaining.cluster
    remaining.mat = res$remaining.mat
    extracted.clusters = NA
  }

  return(list(changed=changed, df=df, remaining.cluster=remaining.cluster, extracted.clusters=extracted.clusters, remaining.mat=remaining.mat))

  
}


# Just a helper function to make the code size
# in equation.system.cluster look smaller. This makes debugging of it easier
old.create.eating.calls = function(env = parent.frame()) {
  calls = list(
    
    # search for equations that have a single variable
    # remove them from the cluster and put them in front
    eat.single.front = quote({
      changed = 0
      while(TRUE) {
        rows = which(df$active & rowSums(mat) == 1)
        if (length(rows)==0) break
        cols = max.col(mat[rows,,drop=FALSE])
        cur.syms = syms[cols]
        if (any(duplicated(cur.syms))) {
          dupl.syms = unique(cur.syms[duplicated(cur.syms)])
          warning(paste0("The variables "), paste0(dupl.syms, collapse=", ")," are determined by more than one equation!")
        }
        
        df$active[rows] = FALSE
        df$cluster[rows] = max(df$cluster)+seq_along(rows)
        df$cluster.size[rows] = 1
        df$level[rows] = max(df$level+1)
        df$var[rows] = cur.syms
        mat[,cur.syms] = 0
        changed = changed + length(rows)
        
        if (verbose) {
          cat("\neat.single.front:")
          cat("\nvars: ", paste0(cur.syms, collapse=", "))
          cat("\nrows: ", paste0(rows, collapse=", "),"\n")
        }
      }
      changed
    }),
    
    # search for systems of equations that only have two variables
    # (the variables must be the same variables)
    # put those equation systems in the front
    eat.double.front = quote ({
      changed = 0
      while(TRUE) {
        rows = which((duplicated(df$vars.id) | duplicated(df$vars.id, fromLast = TRUE)) & rowSums(mat) == 2)
        if (length(rows)==0) break 
        changed = changed + length(rows)
        first = which(duplicated(df$vars.id, fromLast = TRUE))
    
        df$active[rows] = FALSE
        df$cluster.size[rows] = 2
        df$level[rows] = max(df$level)+1
        
        for (row in first) {
          crows = which(df$vars.id==df$vars.id[[row]])
          cur.syms = syms[which(mat[row,]==1)]
          df$cluster[crows] = max(df$cluster)+1
          df$var[crows] = cur.syms
        }
        vars = unique(unlist(var.li[rows]))
        mat[,vars] = 0
           
        level = level+1
        
        if (verbose) {
          cat("\neat.double.front: ")
          cat("\nvars: ", paste0(vars, collapse=", "))
          cat("\nrows: ", paste0(rows, collapse=", "),"\n")
        }
  
      }
      changed
    }),
    
    # Try to remove equations at the back of a cluster
    # search for variables that appear in only one equation
    # this equation must then define this variable
    # we can put the equation behind the larger cluster
    eat.single.back = quote({
      changed = 0
      while(TRUE) {
        # Compute for each symbol the number of equations 
        # it is still contained in
        num.eqs = colSums(mat)
        # Find the symbol columns that are only in a single
        # equation
        cols = which(num.eqs==1)
        if (length(cols)==0) break
        # Identify the equations that have a single column
        rows = which(rowSums(mat[,cols, drop=FALSE]) >= 1)
        later.level = min(df$level)-1
        if (verbose) {
          cat("\neat.single.back: ", later.level, "")
          cat("\nvars: ", paste0(names(cols), collapse=", "))
          cat("\nrows: ", paste0(rows, collapse=", "),"\n")
        }
        df$level[rows] = later.level
        df$active[rows] = FALSE
        df$cluster[rows] = min(df$cluster)-seq_along(rows)
        df$cluster.size[rows] = 1
        for (col in cols) {
          row = which(mat[,col] >= 1)
          df$var[row] = syms[col]
        }
        
        mat[rows,] = 0
        changed = changed + length(rows)
      } 
      changed
    }),
    # Try to remove equations from back
    eat.double.back = quote({
      changed = 0
      while(TRUE) {
        # Compute for each symbol the number of equations 
        # it is still contained in
        num.eqs = colSums(mat)
        # Find the symbol columns that are in exactly two equations
        cols = which(num.eqs==2)
        if (length(cols)<2) break
  
        col.rows = lapply(cols, function(col) {
          rows = which(mat[,col] >= 1)
          sort(rows)
        })
        col.rows = do.call(rbind,col.rows)
        dupl = which(duplicated(col.rows, fromLast = TRUE))
        if (length(dupl)==0) break
        
        level = min(df$level)-1
        for (ind in dupl) {
          inds = which(col.rows[,1] == col.rows[ind,1] & col.rows[,2] == col.rows[ind,2])
          ccols = cols[inds]
          crows = col.rows[ind,]
          
          df$active[crows] = FALSE
          df$cluster.size[crows] = 2
          df$level[crows] = level
        
          df$cluster[crows] = min(df$cluster)-1
          df$var[crows] = syms[ccols]
          
          if (verbose) {
            cat("\neat.double.back:")
            cat("\nvars: ", paste0(syms[ccols], collapse=", "))
            cat("\nrows: ", paste0(crows, collapse=", "),"\n")
          }
          mat[crows,] = 0
          changed = changed + 2
        }
      } 
      changed
    })
  )  
}



examples.replace.symbolic.funs = function() {
  funs = list(yS=quote(tau * K^(kappa) * L^(lambda)), sqr=quote(K_eq^2) )
  call = quote(Diff(yS(L=3, K = sqr(K_eq=5)), kappa))
  call = quote(Diff(yS(),tau))
  
  call = replace.variable.funs(call, funs)
  call
  compute.symbolic.ops(call)
}

compute.symbolic.ops = function(call, recursive = TRUE) {
  restore.point("compute.symbolic.ops")
  if (is.call(call)) {
    name = as.character(call[[1]])
    if (name=="Deriv") {
      restore.point("compute.symbolic.ops.Deriv")
      call = eval(call)
    }
  }  
  if (length(call)>1 & recursive) {
    for (i in seq_along(call)[-1])
      call[[i]] = compute.symbolic.ops(call=call[[i]], recursive=TRUE)
  }
  call
}

replace.variable.funs = function(call, funs, recursive=TRUE) {
  restore.point("replace.variable.funs")
  if (is.call(call)) {
    name = as.character(call[[1]])
    # A call to be replaced
    if (name %in% names(funs)) {
      restore.point("replace.variable.funs.inner")
      args = lapply(seq_along(call)[-1], function(i) call[[i]])
      vars = names(call)
      if (is.null(vars) & length(args)>0) {
        stop(paste0("Error when calling the user defined function ", deparse1(call),":\n You must name all arguments!"))
      } else {
        if (any(vars[-1]=="") & length(args)>0) {
          stop(paste0("Error when calling the user defined function ", deparse1(call),":\n You must name all arguments!"))
        }
      }
      call = funs[[name]]
      if (length(vars)>0) {
        names(args) = vars[-1]
        call = substitute.call(call, args)
      }
    }
  }  
  if (length(call)>1 & recursive) {
    for (i in seq_along(call)[-1])
      call[[i]] = replace.variable.funs(call=call[[i]],funs=funs, recursive=TRUE)
  }
  call
}


# replace functional references to other functions
# and calls to Deriv
compute.equation.funs = function(eqs, funs=NULL) {
  restore.point("compute.equation.funs")
  
  # replace functional references to other functions
  # do not yet check for cycles
  counter = 0
  changed = rep(TRUE,length(eqs))
  while (sum(changed)>0) {
    counter = counter +1
    for (eq.ind in which(changed)) {
      eq = replace.variable.funs(eqs[[eq.ind]], funs)
      changed = !identical(eq, eqs[[eq.ind]])
      if (changed) eqs[[eq.ind]] = eq
    }
    if (counter > 1000) {
      stop("To many function substitutions. You probably have cycles in your function references!")
    }
  }
    
  # replace Deriv functions
  eqs = lapply(eqs,compute.symbolic.ops)
  eqs
}


extract.explicit.eqs = function(eqs) {
  # extract explicitly defined variables
  # they can be referenced as functions
  is.explicit= sapply(seq_along(eqs), function(i) {
    eq = eqs[[i]]
    var = names(eqs[i])
    if (as.character(eq[[2]]) != var) return(FALSE)
    rhs.vars = find.variables(eq[[3]])
    if (var %in% rhs.vars) return(FALSE)
    return(TRUE)
  })
  funs = lapply(eqs[is.explicit], function(eq) eq[[3]])
  funs
}


cluster.df.nice.code = function(df) {
  restore.point("cluster.df.nice.code")
  
  clusters = unique(df$cluster)
  clu = 1
  li = lapply(clusters, function(clu) {
    rows = which(df$cluster == clu)
    vars = df$var[rows]
    solved = df$solved[rows[1]]
    if (solved & length(rows)>1)
        stop("We have a solved cluster with more than 1 row")
    
    if (solved) {
      row = rows
      txt = paste0(
        "\n# Compute ", vars, " from ", deparse1(df$org_[[row]]),
        "\n",vars," = ", deparse1(df$expl_[[row]]),
        "\n",vars
      )    
    } else {
      txt = paste0(
        "\n# Solve ", paste0(vars,collapse=", "), " from ",
        paste0("\n#  ", sapply(df$org_[rows],deparse1),collapse="")
      )
      
      
      xsubst = lapply(seq_along(vars), function(i) {
        substitute(x[i],list(i=i))
      })
      names(xsubst) = vars
      # Create function
      impl_ = lapply(df$eq_[rows], function(eq_) {
        call = substitute(lhs-(rhs), list(lhs=eq_[[2]],rhs=eq_[[3]]))
        call = substitute.call(call, xsubst)
        call
        
      })
      inner = as.call(c(list(as.symbol("c")),impl_))
      fn = function(x) {}
      body(fn) = substitute({inner}, list(inner=inner))
      fn
      
      code = substitute({
        x <- runif(length(endo)) 
        fn <- fun  
        #sol = mynleqslv(x=x, fn=fn)     
        sol = optims(par=x, eq.fn=fn)     
        if (!sol$ok) {
          warning(paste0("Could not solve ", paste0(endo, collapse=", ")," in steady state.\n", sol$message ))
        }
      }, list(fun=fn,endo=vars))
      code.txt = paste0(sapply(code[-1], deparse1, collapse = "\n"), collapse="\n")
      txt = c(txt,"", code.txt)
      if (length(vars)>1) {
        txt = c(txt,
          "x = sol$par",
          paste0(vars, "= x[", seq_along(vars),"]", collapse="; "),
          paste0("c(",paste0(vars,collapse=","),")")
        )
      } else {
        txt = c(txt,
          paste0(vars, "= sol$par"),
          paste0(vars)
        )
      }
    }
    paste0(txt, collapse="\n")
  })

  


  code = paste0(li, collapse="\n\n")

  check.eq =  lapply(df$org_, function(eq) {
      deparse1(eq)
  })
  check.res = lapply(df$org_, function(eq) {
      paste0(deparse1(eq[[2]]),"-(",deparse1((eq[[3]])),")")
  })
  code = paste0(code,
"\n\ncheck.df <- data_frame(
  eq_ = qlist(", paste0(check.eq, collapse=",\n    "), "\n  ),

  lhs_rhs = c(",paste0(check.res, collapse=",\n    "), "\n  )
)
check.df$ok = abs(check.df$lhs_rhs) < 1e-8
check.df
")  

  #cat(code)
  code
}

# Create fairly fast code that computes a cluster.df
adapt.make.inner.compute.code = function(df,  exo=NULL, subst.li=NULL) {
  restore.point("make.inner.compute.code")
  
  clusters = sort(unique(df$cluster))
  clu = 2
  li =lapply(clusters, function(clu) {
    rows = which(df$cluster == clu)
    if (all(df$solved[rows]) & (!all.implicit)) {
      code.li = lapply(rows, function(row) {
        code = substitute(lhs<-rhs, 
             list(lhs=as.symbol(df$var[[row]]),rhs=df$expl_[[row]]))
        if (!is.null(subst.li))
          code = substitute.call(code, subst.li)
        code
      })
      return(code.li)
    } else {
      endo = df$var[rows]

      xsubst = lapply(seq_along(endo), function(i) {
        substitute(x[i],list(i=i))
      })
      names(xsubst) = endo

      # substitute original impl_ formula with x, par.mat and var.mat
      impl_ = lapply(df$impl_[rows], function(call) {
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

extract.cluster.df.dummies = function(df) {
  rows = str.starts.with(df$var,"DUMMY___")
  return(list(df=df[!rows,,drop=FALSE], test.eqs = df$org_[rows]))
}

