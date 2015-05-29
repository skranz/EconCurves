
sympy.to.r = function(str) {
  gsub("**","^",str,fixed=TRUE)    
}

r.to.sympy = function(str=NULL,call=NULL) {
  if (!is.null(call))
    str = deparse1(call)
  gsub("^","**",str,fixed=TRUE)  
}
example.r.to.sympy = function() {
  r.to.sympy(c("x^5=5","y^2=3"))
  
}


set.sympy.symbols = function(str=NULL,call=NULL, vars=NULL) {
  if (!is.null(str))
    call = parse(text=str)
  if (is.null(vars))
    vars = find.variables(call)
  sympy(paste0("var('",paste0(vars,collapse=" "),"')"))
}


latex = function(str) {
  str = r.to.sympy(str)
  set.sympy.symbols(str)
  sympy(paste0("latex(",str,")"))
}

          
to.sympy.list = function(var, quotes="") {
  paste0("[",paste0(quotes,r.to.sympy(var),quotes,collapse=","),"]")
}


examples.sympy.solve.eq = function() {
  sympy.solve.eq(eq=quote((1+3+4*x)+z == 5),"x")
  sympy.solve.eq(eq=quote(x^2 == 1),"x")

  eqs = list(
    quote(x+y==5),
    quote(y+5*x+y==x+z)
  )
  vars = c("x,","y")
  sympy.solve.eqs(eqs, vars=c("x","y"))

}

sympy.solve.eq = function(eq,var) {
  restore.point("sympy.solve.eq")
  library(rSymPy)
  sympy.solve.eqs(eq = list(eq), vars=var)
}

sympy.solve.eqs = function(eqs,vars, impls_=NULL) {
  restore.point("sympy.solve.eqs")
  library(rSymPy)
  
  if (length(impls_)==0) {
    impls_ = lapply(eqs, function(eq) {
      substitute(lhs-(rhs), list(lhs=eq[[2]],rhs=eq[[3]]))
    })
  }
  symbols = unique(unlist(lapply(impls_, find.variables)))
  
  set.sympy.symbols(vars=symbols)
  r.to.sympy(call=impls_[[1]])
  
  impls = sapply(impls_, function(impl_) r.to.sympy(call=impl_))
  code = paste0('solve(',to.sympy.list(impls),',',to.sympy.list(vars),')')
  
  code = "solve([x**2 - (1)==4, y-3==2],[x,y])"
  sympy(code)
  res.str = sympy(code)
  expl = sympy.parse.result(res.str)
  
  if (!is.null(expl)) {
    return(list(solved=TRUE, expl=expl))
  } else {
    return(list(solved=TRUE, expl=NULL))
  }
}

# returns passed elements as a list of quoted calls
qlist = function(...) {
  eval(substitute(alist(...)))
}


sympy.parse.result = function(str, silent=FALSE) {
  str = gsub("**","^",str,fixed=TRUE)

  str = gsub("]",')',str,fixed=TRUE)
  str = gsub("}",')',str,fixed=TRUE)
  str = gsub("[",'qlist(',str,fixed=TRUE)
  str = gsub("{",'qlist(',str,fixed=TRUE)
  str = gsub(":",'=',str,fixed=TRUE)
  str
  
  
  res = try(silent=silent,{
    call = parse.as.call(str)
    eval_qlist(call)
  })
  if (is(res,"try-error")) return(NULL)
  res
} 

eval_qlist = function(call,...) {
  if (is.symbol(call)) return(call)
  if (call[[1]]=="qlist") {
    li = eval(call,...)
    li = lapply(li, eval_qlist,...)
    return(li)
  }
  return(call)
}