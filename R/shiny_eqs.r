examples.shiny.eqs = function() {
  set.restore.point.options(display.restore.point = TRUE)
  
  rows = which(df$cluster.size==max(df$cluster.size))
  cluster = df$cluster[rows[1]]
  rows = which(df$cluster==cluster)
  eqs = df$eq_[rows]
  vars = df$var[rows]
  vals = exo.vals
  
  se = make.se(se.ind=1,eqs=eqs,vars=vars, vals=vals)
  
  app = eventsApp()

  eqs.ui = se.subst.ui(se=se)
  app$ui = fluidPage(eqs.ui)
  runEventsApp(app, launch.browser = rstudio::viewer)
  
  ui 
  
  
  cluster = which(df$cluster)
  
  clu.df
  exo
  cluster = unique(clu.df$cluster)
  li = lapply(cluster, cluster.ui, clu.df=clu.df)
  exo.ui = exo.ui(exo)
  ui = fluidPage(exo.ui,hr(), li)
  
  app = eventsApp()
  app$ui = ui
  runEventsApp(app, launch.browser = rstudio::viewer)
  
}

click.se.subst.btn = function(app=getApp(), se, var, eq.ind,...) {
  restore.point("click.ses.subst.btn")
  
  res = symbolic.subst.var(eqs = se$eqs,var = var,eq.ind = eq.ind)

  if (res$ok) {
    nse = make.se(eqs = res$eqs,vars = setdiff(se$vars,var),vals=se$vals,se.ind=se$se.ind+1,exo = se$exo,subst.eqs = c(list(res$subst.eq),  se$subst.eqs),subst.vars = c(var,se$subst.vars),prev.se = se)
    se$next.se = nse
    ui = se.subst.ui(nse)
    setUI(paste0("seSubstUI",se$se.ind+1), ui)
  } else {
    setUI(paste0("seSubstUI",se$se.ind+1), HTML("<bold> Could not solve equation!</bold>"))
  }
  
}


se.subst.ui = function(se, simplify.fun=Deriv::Simplify) {
  restore.point("eqs.subst.ui",deep.copy = FALSE)

  eqs=se$eqs
  subst.eqs=se$subst.eqs
  vars=se$vars
  vals = se$vals 
  
  eq.ind = 1
  
  uis = lapply(seq_along(eqs), function(eq.ind) {
    eq = eqs[[eq.ind]]  
    syms = find.variables(eq)
    endo = intersect(syms, vars)
    eq.str = colored.deparse(eq, endo)

    buttons = lapply(endo, function(var) {
      btn.id = paste0("eqsSubst",se$se.ind,"Btn__",var,"__",eq.ind)
      btn = bsButton(inputId = btn.id,label = var,size = "extra-small")
      q_eqs = lapply(eqs, function(eq) substitute(quote(eq), list(eq=eq)))
      buttonHandler(btn.id, click.se.subst.btn, se=se, var=var, eq.ind)
      btn
    })
    list(HTML(eq.str),buttons,br())
  })
  
  if (!is.null(vals)) {
    cols = setdiff(names(vals), vars)
    valsubst = as.list(vals[cols])
  
    
    nuis = lapply(seq_along(eqs), function(eq.ind) {
      eq = eqs[[eq.ind]]  
      syms = find.variables(eq)
      endo = intersect(syms, vars)
      neq = substitute.call(eq,valsubst)
      if (!is.null(simplify.fun))
        neq = simplify.fun(neq)
      
      neq.str = paste0(colored.deparse(neq,endo))
      list(HTML(neq.str),br())
    })
    nuis = c(list(h5("Numeric: ")), nuis)  
  } else {
    nuis=NULL
  }

  suis = lapply(subst.eqs, function(eq) {
    eq.str = deparse1(eq)
    if (!is.null(vals)) {
      rhs = eq[[3]]
      rhs = substitute.call(rhs, as.list(vals))
      if (!is.null(simplify.fun))
        rhs = simplify.fun(rhs)

      eq.str = paste0(eq.str, " = ", deparse1(rhs))
    }
    list(HTML(eq.str),br())
  } )
  list(h3(paste0("Level ", se$se.ind)), uis,nuis,h5("Substituted:"),suis,uiOutput(paste0("seSubstUI",se$se.ind+1)))
}


colored.deparse = function(call,vars, color="blue") {
  subst = lapply(vars, function(var) as.name(paste0("..SBLUE..",var,"..EBLUE..")))
  names(subst) = vars
  call = substitute.call(call, subst)
  str = deparse1(call)
  str = gsub("..SBLUE..","<font color='blue'>",str, fixed=TRUE)
  str = gsub("..EBLUE..","</font>",str, fixed=TRUE)
  str
}



exo.ui = function(exo) {
  list(
    h4("Exogenous variables"),
    p(paste0(names(exo)," = ", exo, collapse=", "))
  )
}

cluster.ui = function(cluster,clu.df,exo=NULL) {
  
  df = clu.df
  rows = which(df$cluster==cluster)
  #df = clu.df[clu.df$cluster==cluster,]
  endo = df$var[rows]  
  solved = all(df$solved[rows])
  
  if (length(rows)>1) {
    title = h5(paste0("  Cluster ", cluster, " (",length(rows),")"))
  } else {
    title = NULL
  }
  
  str = paste0("    ",rows,". ",df$var[rows],"=",signif(df$val[rows],3), " : ", sapply(df$eq_[rows],colored.deparse, vars=endo))
  if (TRUE | any(!is.finite(df$val[rows]))) {
    brows = 1:(min(rows)-1)
    vals = c(exo,df$val[brows] )
    subst = lapply(vals, signif, digits=4)
    names(subst) = c(names(exo),df$var[brows])
    num.eqs = lapply(df$eq_[rows], substitute.call, env=subst)
    num.eqs = sapply(num.eqs,colored.deparse, vars=endo)
    str = paste0(str, "  <=>  ", num.eqs)
  }
  return(list(
    title,
    HTML(paste0(str,"<br>")),
    hr()      
  ))

  

  str.eqs = sapply(df$eq_, colored.deparse, vars=endo)
  list(
    h4(paste0(cluster,ifelse(solved,"","*"), ": ", paste0(df$var," = ",sapply(df$val,signif, digits=4), collapse=", "))),
    HTML(paste0(str.eqs,"<br>")),
    hr()      
  )
}


make.se = function(se.ind,eqs,vars, exo=NULL, vals=NULL, subst.eqs=NULL, subst.vars=NULL, prev.se=NULL, solve.numerical=!is.null(vals)) {
  restore.point("make.se")
  if (solve.numerical) {
    ret = suppressWarnings(numerical.solve.eqs(eqs,vars,exo=vals))
    if (ret$ok) {
      vals[names(ret$par)] = ret$par  
    }
  }
  vals = vals[!is.na(vals)]
  se = as.environment(list(
    se.ind = se.ind,
    eqs = eqs,
    vars= vars,
    exo = exo,
    vals = vals,
    subst.eqs=subst.eqs,
    subst.vars = subst.vars,
    prev.se = prev.se,
    next.se = NULL
  ))
  
  se
}

