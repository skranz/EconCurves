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
#
# lagDependsOn:
#     all previous / lagged variables, a variable depends on
#
# leadDependsOn:
#
# curDependsOn:
#     all current variables that a variable dependsOn
#

examples.model.dependencies = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("AdaptivePricesRandom")
  em = load.model("ThreeEq")
  
  init.model(em)
  em$vars = init.formula.partners(em,em$vars)
  init.vars.formulas(em)
  
  #init.model.scen(em)
  em$init.var
  em$sim = simulate.model(em,T = 200)
  c(sd(sim$p),sd(sim$p_adapt),sd(sim$p_fund))

}

find.model.deps = function(em) {
  deps = lapply(em$vars,function(var) {
    expl_ = em$expl_[[var]]
    impl_ = em$impl_[[var]]
    
    explicit = !is.null(expl_)
    
    
  })
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

init.vars.formulas = function(em) {
  restore.point("init.vars.formulas")
  # phase t
  li = lapply(names(em$vars), function(var) {
    restore.point("fbfhbhfbh")
    fo = init.var.formula(em,fo=em$vars[[var]],var=var, phase="t", list.wrap=TRUE)
    fo
  })
  fo.df = as_data_frame(rbindlist(li))
  names(fo.df$eq_) = names(fo.df$expl_) = names(fo.df$impl_) = 
  names(fo.df$dependsOn) = names(fo.df$curDependsOn) = names(fo.df$lagDependsOn) =
  names(fo.df$leadDependsOn) = fo.df$var
  
  em$fo.df = fo.df
  
}

init.var.formula = function(em, fo, var=fo$name, phase=c("t","init","laginit")[1], list.wrap=FALSE) {
  restore.point("init.var.formula")
  
  type = get.var.formula.type(fo)
  expl_ = NULL
  if (!is.null(fo$formula)) {
    expl_ = parse.as.call(fo$formula)
  }

  if (type == "xcurve" | type =="ycurve") { 
    curve.name = fo[[type]]
    curve = em$curves[[curve.name]]
  }  
   
  if (type== "xcurve") { 
    curve.name = fo[[type]]
    curve = em$curves[[curve.name]]
    eq_ = subst.var(curve$eq_, var = curve$yvar, subs = fo$y, subset=FALSE)
    eq_ = subst.var(eq_, var = curve$xvar, subs = var, subset=FALSE)
  } else if (type=="ycurve") {
    curve.name = fo[[type]]
    curve = em$curves[[curve.name]]
    eq_ = subst.var(curve$eq_, var = curve$yvar, subs = fo$x, subset=FALSE)
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
  }

  impl_ = substitute(lhs-(rhs),list(lhs=get.lhs(eq_),rhs=get.rhs(eq_)))

  if (list.wrap) {
    res = list(var=var,type=type,expl_=list(expl_),impl_=list(impl_),eq_=list(eq_))
  } else {
    res = list(var=var,type=type,expl_=expl_,impl_=impl_,eq_=eq_)
  }
  
  dep.li = compute.var.dependsOn(em,var, impl_, list.wrap=list.wrap)
  
  c(res,dep.li)
}


get.var.formula.type = function(fo) {
  restore.point("get.var.formula.type")
  types = c("xcurve","ycurve","formula","xcut","ycut")
  not.null = sapply(types, function(type) {!is.null(fo[[type]])})
  types[not.null]
}

compute.var.dependsOn = function(em,var, formula_ = em$impl_[[var]], list.wrap=FALSE) {
  dependsOn    = unique(setdiff(codeUtils::find.variables(formula_),var))
  lagDependsOn = dependsOn[str.starts.with(dependsOn,"lag_")]
  leadDependsOn = dependsOn[str.starts.with(dependsOn,"lead_")]
  curDependsOn = setdiff(dependsOn, c(lagDependsOn,leadDependsOn))
  
  if (list.wrap) {
    list(dependsOn=list(dependsOn),curDependsOn=list(curDependsOn),
         lagDependsOn=list(lagDependsOn),leadDependsOn=list(leadDependsOn))
  } else {
    nlist(dependsOn,curDependsOn,lagDependsOn,leadDependsOn)
  }
}

