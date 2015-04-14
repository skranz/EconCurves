
examples.model = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  res = load.model("ThreeEq")
  tt = res$tt
  em = res$em
  init.model(em)
  init.model.scen(em)
  em$init.var
}


init.model = function(em) {
  init.model.curves(em)  
  init.model.vars(em)
}

init.model.scen = function(em, scen.name = names(em$scenarios)[1]) {
  restore.point("init.model.scen")
  
  
  scen = em$scenarios[[scen.name]]
  
  env = new.env()
  for (par in names(scen$init)) {
    val = scen$init[[par]]
    if (is.character(val)) {
      val = eval(parse(text=val),new.env)
    } else {
      attributes(val)=NULL
    }
    env[[par]] = val
  }
  scen$init.par = as.list(env)
  em$scen = scen
  model.initial.var(em)
}

model.initial.var = function(em, round=8) {
  impl_ss_ = c(
    lapply(em$vars, function(var) {var$impl_ss_})
  )
  vars = names(em$vars) 
  par = em$scen$init.par
  
  code = paste0("function(x) {\n",
    paste0("  ",names(par),"=",par, collapse="\n"),"\n\n",
    paste0("  ",vars,"=x[",seq_along(vars),"]", collapse="\n"),"\n",
    "  c(", paste0(sapply(impl_ss_,deparse,width.cutoff = 500L), collapse=","),")\n",
    "}"
  )
  cat(code)
  fn = eval(parse(text=code))
  
  x = rep(0,length(vars))
  res = nleqslv(x,fn)$x
  
  names(res) = vars
  if (!is.null(round))
    res = round(res)
  em$init.var = as.list(res)
  invisible(em$init.var)
}

# not yet implemented
make.model.jacobi = function(em) {
  ex = do.call(expression, em$impl)
  lapply(em$impl,function(call) {
    deriv(call,vars)
  })

}


init.model.curves = function(em) {
  restore.point("init.model.curves")
  
  curve = em$curves[[1]]
  em$curves = lapply(em$curves, function(curve) {
    curve$eq_ = parse.as.call(text=curve$eq)
    curve$impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(curve$eq_),rhs=get.rhs(curve$eq_)))
    
    curve$xvar = curve$xy[1]
    curve$yvar = curve$xy[2]
    
    curve
  })
  invisible(em$curves)
}
subst.var = function(call, var, subs, subset=FALSE) {
  restore.point("substitute.variable")
  if (!is.character(var)) var = deparse(var)
  if (is.character(call)) call = parse(text=call)[[1]]
  if (is.character(subs)) subs = parse(text=subs)[[1]]
  
  sub.li = list(subs)
  names(sub.li) = var
  
  res = substitute.call(call, sub.li)
  #if (subset) res = res[[1]]
  res
}
init.model.vars = function(em) {
  var = em$variables[[1]]
  vars = lapply(em$vars, function(var) {
    restore.point("hfhhfuehuh")
    
    
    var$type = get.model.var.type(var)
    var$name = attr(var,"name")
    if (var$type == "xcurve" | var$type =="ycurve") {
      var$curve = var[[var$type]]
      curve = em$curves[[var$curve]]
      
      if (var$type == "xcurve") {
        var$eq_ = subst.var(curve$eq_, var = curve$yvar, subs = var$y, subset=FALSE)
        var$eq_ = subst.var(var$eq_, var = curve$xvar, subs = var$name, subset=FALSE)
      } else if (var$type == "ycurve") {
        var$eq_ = subst.var(curve$eq_, var = curve$xvar, subs = var$x, subset=FALSE)
        var$eq_ = subst.var(var$eq_, var = curve$yvar, subs = var$name, subset=FALSE)
      }
    } else if (var$type == "xaxis") {
      var$eq = paste0(var$name,"==",var$x)
      var$eq_ = parse.as.call(var$eq)      
    } else if (var$type == "yaxis") {
      var$eq = paste0(var$name,"==",var$y)
      var$eq_ = parse.as.call(var$eq)      
    }
    # Implicit equation
    var$impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(var$eq_),rhs=get.rhs(var$eq_)))

    # Condition for steady state
    ivars = find.variables(var$impl_)
    lagged = ivars[str.starts.with(ivars,"lag_")]
    if (length(lagged)>0) {
      unlagged = str.right.of(lagged,"lag_")
      var$impl_ss_ = subst.var(var$impl_,var = lagged, subs = unlagged, subset=FALSE)
    } else {
      var$impl_ss_ = var$impl_
    }
    
    var

  })
  
  em$vars = vars
  invisible(em$vars)
}
get.model.var.type = function(var) {
  restore.point("get.model.var.type")
  types = c("xcurve","ycurve","xaxis","yaxis")
  not.null = sapply(types, function(type) {!is.null(var[[type]])})
  types[not.null]
}

make.model.eq = function(em) {
  curves.eq = lapply(em$curves, function(curve) curve$eq)
  
  
}

run.model = function(em) {
  
}


