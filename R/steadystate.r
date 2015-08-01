
examples.steady.state = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("Capital3Eq")
  check.model(em)
  
  init.model(em,solve.systems = !TRUE)
  init.model.scen(em = em)
  res = solve.steady.state(em)
  
  sim = simulate.model(em)
  

}


solve.steady.state = function(em, scen=em$scen) {
  restore.point("solve.steady.state")
  
  init = scen$init
  init.syms = names(init)
  
  so.df = em$fo.df
  vars = so.df$var
  pars = em$par.names
  
  # Find lag variables that will be replaced
  # by their non-lag counterpart, since they are
  # not in the steady state
  so.syms = unique(unlist(lapply(so.df$eq_, function(eq_) find.variables(eq_))))
  so.lag = so.syms[str.starts.with(so.syms,"lag_")]
  replace.lag = setdiff(so.lag, init.syms)
  
  replace.li = lapply(replace.lag, function(sym) as.name(str.right.of(sym,"lag_")))
  names(replace.li) = replace.lag

  # replace lags  
  so.eqs = lapply(so.df$eq_, function(eq_) {
    substitute.call(eq_, replace.li)
  })
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
  
  clu.df = cluster.equations(eqs, exo=exo)
  clu.df

  
  txt = cluster.df.nice.code(clu.df)
  txt = c(
    "# Init exogenous symbols",
    paste0(number.init," = ", init[number.init],collapse="; " ),
    txt
  )
  txt = paste0(txt, collapse="\n\n")
  writeClipboard(txt)
  list(code=code,clu.df=clu.df)
}

cluster.df.nice.code = function(df) {

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
        sol = mynleqslv(x=x, fn=fn)     
        if (max(abs(sol$fvec))> 1e-07) {
          warning(paste0("Could not solve ", paste0(endo, collapse=", ")," in steady state: max deviation = ",max(abs(sol$fvec)) ))
        }
      }, list(fun=fn,endo=vars))
      code.txt = paste0(sapply(code[-1], deparse1, collapse = "\n"), collapse="\n")
      txt = c(txt,"", code.txt)
      if (length(vars)>1) {
        txt = c(txt,
          "x = sol$x",
          paste0(vars, "= x[", seq_along(x),"]", collapse="; "),
          paste0("c(",paste0(vars,collapse=","),")")
        )
      } else {
        txt = c(txt,
          paste0(vars, "= sol$x"),
          paste0(vars)
        )
      }
    }
    paste0(txt, collapse="\n")
  })

  code = paste0(li, collapse="\n\n")
  #cat(code)
  code
}

cluster.equations = function(eqs, var.li=NULL, exo=NULL, verbose=TRUE) {
  restore.point("cluster.equations")

  # Find for each formula the contained endogenous variables
  if (is.null(var.li)) {
    var.li = lapply(eqs, function(form) {
      vars = find.variables(form)
      setdiff(vars, exo)
    }) 
  }
  syms = unique(unlist(var.li))

  nr = length(var.li); nc = length(syms)
  mat = matrix(0,nr,nc)
  colnames(mat) = syms
  for (i in seq_along(var.li)) {
    mat[i,var.li[[i]]] = 1
  }
  mat
  
  df = data_frame(eq.ind = 1:nr, eq_ = eqs, expl_ = vector("list",nr),level=0,cluster=0,cluster.size=0, active=TRUE, vars=var.li, vars.id = sapply(var.li, function(vars) paste0(sort(vars), collapse="|")), num.vars=sapply(var.li, length), sym="")

  create.eating.calls()
  
  sum.changed = 1
  while(sum.changed>0) {
    # Find systems with 1 variable
    eval(eat.single.front)
    sum.changed = changed
    eval(eat.double.front)
    sum.changed = sum.changed + changed
    eval(eat.single.back)
    sum.changed = sum.changed + changed
    eval(eat.double.back)
    sum.changed = sum.changed + changed
  }
  
  # put remaining rows into a cluster
  rows = which(df$cluster==0)
  if (length(rows)>0) {
    remain.syms = setdiff(syms, df$sym)
    df$cluster[rows] = max(df$cluster)+1
    df$cluster.size[rows] = length(rows)
    df$sym[rows] = remain.syms    
    df$level[rows] = max(df$level)+1
#    df$active[rows] = FALSE
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
  
  # try to symbolically reduce clusters with more than one equation
  bigclus = unique(df$cluster[df$cluster.size>1])
  
  li = lapply(unique(df$cluster), function(clu) {
    rows = which(df$cluster == clu)
    eqs = df$eq_[rows]
    endo = df$sym[rows]
    clu.df = symbolic.cluster.equations(eqs,endo=endo)
    clu.df$org.row = rows
    
    frac.level = 1 + (clu.df$level / length(unique(clu.df$level)))
    clu.df$cluster = clu + frac.level
    clu.df$level = df$level[rows[1]] + frac.level
    clu.df
  })
  df = rbindlist(li)
  df$cluster = rank(df$cluster,ties.method = "min")
  df = df[order(df$cluster),]

  
  df
}



symbolic.cluster.equations = function(eqs, exo=NULL, endo=NULL, eq.solve.fun=sym.solve.eq, level=-1) {
  restore.point("symbolic.cluster.equations")
  
  if (length(eqs)==3) restore.point("nfjdmfgdkgrngzbgvf")
  if (is.null(endo)) {
    vars = unique(unlist(lapply(eqs, find.variables)))
    endo = setdiff(vars, exo)
  }  

  if (length(endo) != length(eqs)) {
    stop("number of endo variables and number of equations is not identical")
  }
  
  # check if one equation is already in form of an explicit solution
  solved = FALSE
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
  
  # could solve an equation
  if (solved) {
    expl_ = new.eq[[3]]
    
    # try to solve remaining equations,
    # plugging in the solved value
    if (length(eqs)>1) {
      #restore.point("symbolic.cluster.equations.outer")
      reqs = eqs[-eq.ind]
      
      # substitute solution into remaining equations
      subst.li = list(substitute((expl_),list(expl_=expl_)))
      names(subst.li) = var
      reqs = lapply(reqs, substitute.call,  env=subst.li)
      
      rdf = symbolic.cluster.equations(reqs, endo=setdiff(endo,var),eq.solve.fun = eq.solve.fun,level=level-1)
    } else {
      rdf = NULL
    }
    eq.df = data_frame(var=var,expl_=list(expl_), eq_ = list(new.eq), org_ = eqs[eq.ind], cluster.size = 1, solved=TRUE, level=level)
    df = rbind(rdf, eq.df)
  } else {
    df = data_frame(var=endo,expl_=vector("list",length(endo)), eq_ = eqs, org_= eqs, cluster.size = length(endo), solved=FALSE, level=level)
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

# Just a helper function to make the code size
# in equation.system.cluster look smaller. This makes debugging of it easier
create.eating.calls = function(env = parent.frame()) {
  
  env$eat.single.front = quote({
    changed = 0
    while(TRUE) {
      rows = which(df$active & rowSums(mat) == 1)
      if (length(rows)==0) break
      cols = max.col(mat[rows,,drop=FALSE])
      cur.syms = syms[cols]
      
      df$active[rows] = FALSE
      df$cluster[rows] = max(df$cluster)+seq_along(rows)
      df$cluster.size[rows] = 1
      df$level[rows] = max(df$level+1)
      df$sym[rows] = cur.syms
      mat[,cur.syms] = 0
      changed = changed + length(rows)
      
      if (verbose) {
        cat("\neat.single.front:")
        cat("\nvars: ", paste0(cur.syms, collapse=", "))
        cat("\nrows: ", paste0(rows, collapse=", "),"\n")
      }
    }
    changed
  })
  env$eat.double.front = quote ({
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
        df$sym[crows] = cur.syms
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
  })
  
  # Try to remove equations from back
  env$eat.single.back = quote({
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
        df$sym[row] = syms[col]
      }
      
      mat[rows,] = 0
      changed = changed + length(rows)
    } 
    changed
  })

  # Try to remove equations from back
  env$eat.double.back = quote({
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
        df$sym[crows] = syms[ccols]
        
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

