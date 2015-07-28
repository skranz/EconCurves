optims = function(par, fn=NULL, ineq.fn=NULL, eq.fn=NULL, target=NULL, lower=NULL, upper=NULL, gr=NULL, algos = selector(par=par,fn=fn,gr=gr, ineq.fn=ineq.fn,eq.fn=eq.fn,lower=lower,upper=upper),   selector = select.base.algos, tol = 1e-8, constr.tol = 1e-12, try.all.algos = TRUE, store.summary=try.all.algos,...) {
  args = list(...)

  restore.point("optims")

  #do.call(fn, c(args,list(x=rep(5,args$T))))
    
  n = length(algos)
  
  res.li = vector("list",n)
  values = rep(Inf, n)
  

  i = 1
  for (i in 1:n) {
    algo = algos[[i]]
    act.target = target
    if (algo$ineq2fun | algo$eq2fun) act.target = 0
    optim.fun = get.algo.fun(algo)
    start.time = Sys.time()
    res = optim.fun(par=par,fn=fn,ineq.fn=ineq.fn,eq.fn=eq.fn,lower=lower,upper=upper,gr=gr,...)
    res$runtime = Sys.time()-start.time 
    res$algo = algo
    if (!is.null(act.target)) {
      if (!is.true(abs(res$value-act.target)<=tol))
        res$ok = FALSE
    }
    
    if (!try.all.algos & res$ok) return(res)
    
    values[i] = res$value
    res.li[[i]] = res
  }  
  restore.point("optims2")

  if (store.summary) {
    li = lapply(res.li, function(res) {
      quick.df(name=paste0(res$algo$base," ",res$algo$method), ok=res$ok, runtime=res$runtime, value=res$value)
    })
    df = bind_rows(li)
    df = arrange(df,-ok,runtime)
    assign(x = "optims.perf.df",value = df,envir = .GlobalEnv)
    #print(df)
  }
  
  best.i=which.min(values)
  res.li[[i]]
}

select.base.algos = function(par,fn=NULL, ineq.fn=NULL, eq.fn=NULL, lower=NULL, upper=NULL, jacobi=NULL, hessian=NULL, ...) {
  restore.point("select.base.algos")

  ineq2fun=FALSE
  eq2fun=FALSE
  if (is.null(fn)) {
    if (!is.null(ineq.fn))
      ineq2fun = TRUE
    if (!is.null(eq.fn))
      eq2fun = TRUE
  }
  
  
  # Constraint optimization with equality or ineqality constraints
  if (!is.null(fn) & !is.null(ineq.fn) | !is.null(eq.fn)) {
    return(list(
      #constrOptimNL = nlist(base="alabama",method="constrOptim.nl",ineq2fun,eq2fun)
      #solnp = nlist(base="Rsolnp",method="solnp",ineq2fun,eq2fun)
      cobyla = nlist(base="cobyla",method="",ineq2fun,eq2fun)
    ))
  }
  
  
  if (length(par)==1) {
    if (is.null(lower) | is.null(upper)) {
      return(list(UnboundedBrent = nlist(base="UnboundedBrent",ineq2fun,eq2fun)))
    } else {
      return(list(
        Brent = nlist(base="optim",method="Brent",ineq2fun,eq2fun),
        L_BFGS_B = nlist(base="optim", method="L-BFGS-B",ineq2fun,eq2fun)
      ))
    }
  }
  
  if (!is.null(lower) | !is.null(upper)) {
    first = list(L_BFGS_B = nlist(base="optim", method="L-BFGS-B",ineq2fun,eq2fun))
  } else {
    first = list(BFGS = nlist(base="optim", method="BFGS",ineq2fun,eq2fun))
  }
  second = list(
    NelderMead=nlist(base="optim",method="Nelder-Mead",ineq2fun,eq2fun)
  )
  c(first,second)
}

get.algo.fun = function(algo) {
  restore.point("get.algo.fun")
  base = algo$base
  method = algo$method  
  
  if (base=="alabama") {
    if (method == "constrOptim.nl") {
      fun = function(par, fn=NULL,ineq.fn=NULL, eq.fn=NULL, gr=NULL, ineq.jacobi=NULL, eq.jacobi=NULL,lower=NULL, upper=NULL,...) {
        restore.point("constrOptim.nl.fun")
        res = alabama::constrOptim.nl(par=par, fn=fn,  gr=gr, hin=ineq.fn, hin.jac=ineq.jacobi, heq=eq.fn, heq.jac=eq.jacobi,...)
        restore.point("optim.fun.inner")
        list(par=res$par, value=res$value, ok=res$convergence==0, counts=res$counts, message=res$message, org=res)
      }
    }
  } else if (base=="cobyla") {
    fun = function(par, fn=NULL,ineq.fn=NULL, eq.fn=NULL, gr=NULL, ineq.jacobi=NULL, eq.jacobi=NULL,lower=NULL, upper=NULL,...) {
      restore.point("cobyla.fun")
      res = nloptr::cobyla(x0=par, fn=fn, hin=ineq.fn, lower=lower, upper=upper,nl.info=TRUE,...)
      list(par=res$par, value=res$value, ok=res$convergence>=0, counts=res$iter, message=res$message, org=res)
    }
  } else if (base=="optim") {
    if (is.null(method)) method = "Nelder-Mead"
    fun = function(par, fn=NULL,ineq.fn=NULL, eq.fn=NULL, lower=NULL, upper=NULL, hessian, jacobi,...) {
      if (is.null(lower)) lower =-Inf
      if (is.null(upper)) upper = Inf
      fn = transform.optim.fn(fn, ineq.fn,eq.fn)
      res = optim(par=par, fn=fn, method=method, lower=lower, upper=upper,...)
      restore.point("optim.fun.inner")
      list(par=res$par, value=res$value, ok=res$convergence==0, counts=res$counts, message=res$message, org=res)
    }
  } else if (base=="UnboundedBrent") {
    fun = function(par, fn=NULL,ineq.fn=NULL, eq.fn=NULL, lower=NULL, upper=NULL, hessian, jacobi,...) {
      fn = transform.optim.fn(fn, ineq.fn,eq.fn)
      res = unbounded.brent(par=par, fn=fn, ...)
      restore.point("optim.fun.inner")
      list(par=res$par, value=res$value, ok=res$convergence==0, counts=res$counts, message=res$message, org=res)
    }
  } else {
    stop("unknown base ", base)
  }
  fun
}

unbounded.brent = function(par, fn, lower=-Inf, upper=Inf, bound.tolerance=0.01, ...) {
  restore.point("unbounded.brent")
  
  if (is.null(lower)) lower = -Inf
  if (is.null(upper)) upper = Inf
  
  init.par = par
  bound = 0.01
  counter = 0
  ok = FALSE
  while(counter<30) {
    counter = counter+1
    res = optim(init.par, fn,method="Brent",lower=max(-bound,lower), upper=min(bound,upper),...)
    par = res$par
    if ( abs((abs(res$par)-bound) / bound) <= 0.01 ) {
      bound = bound*10
    } else {
      ok = TRUE
      break
    }
  }
  if (!ok) {
    res$convergence = 100
    res$message = "No convergence to any tried bound"
  }
  res
}

# transform the optimization function fn
# for the case that we just solve a problem with constraints
transform.optim.fn = function(fn=NULL,ineq.fn=NULL, eq.fn=NULL) {
  restore.point("transform.optim.fn")
  if (!is.null(fn)) return(fn)
  if (!is.null(eq.fn) & !is.null(ineq.fn)) {
    fn = function(...) {
      c1 = ineq.fn(...)
      c2 = eq.fn(...)
      sum(c(c1,c2)^2)
    }
  } else if (!is.null(ineq.fn)) {
    fn = function(...) {
      c1 = ineq.fn(...)
      sum(c1^2)
    }
  } else if (!is.null(eq.fn)) {
    fn = function(...) {
      c1 = eq.fn(...)
      sum(c1^2)
    }
  }
  return(fn)
}

