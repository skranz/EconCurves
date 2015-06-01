optims = function(par, fn, lower=NULL, upper=NULL, jacobi=NULL, hessian=NULL, algos = selector(par,lower,upper,jacobi,hessian),   selector = select.base.algos, ...) {
  restore.point("optims")
  
  optim.fun = get.algo.fun(algos[[1]])
  optim.fun(par,fn,lower,upper,hessian, jacobi,...)
}

select.base.algos = function(par, lower=NULL, upper=NULL, jacobi=NULL, hessian=NULL, ...) {
  restore.point("select.base.algos")
  
  if (length(start)==1) {
    if (is.null(lower) | is.null(upper)) {
      return(list(UnboundedBrent = list(base="UnboundedBrent")))
    } else {
      return(list(Brent = list(base="optim",method="Brent")))
    }
  }
  
  if (!is.null(lower) | !is.null(upper)) {
    first = list(L_BFGS_B = list(base="optim", method="L-BFGS-B"))
  } else {
    first = list(BFGS = list(base="optim", method="BFGS"))
  }
  second = list(
    NelderMead=list(base="optim",method="Nelder-Mead")
  )
  c(first,second)
}

get.algo.fun = function(algo) {
  restore.point("get.algo.fun")
  base = algo$base
  if (base=="optim") {
    method = algo$method
    if (is.null(method)) method = "Nelder-Mead"
    fun = function(par, fn, lower=NULL, upper=NULL, hessian, jacobi,...) {
      if (is.null(lower)) lower =-Inf
      if (is.null(upper)) upper = Inf
      res = optim(par=par, fn=fn, method=method, lower=lower, upper=upper,...)
      list(par=res$par, value=res$value, ok=res$convergence==0, counts=res$counts, message=res$message, org=res)
    }
  } else if (base=="UnboundedBrent") {
    fun = function(par, fn, lower=NULL, upper=NULL, hessian, jacobi,...) {
      res = unbounded.brent(par=par, fn=fn, ...)
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
