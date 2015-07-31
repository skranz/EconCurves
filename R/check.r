check.story = function(file=NULL, es=NULL) {
  restore.point("check.story")
  
  if (is.null(es))
    es <- load.story(file = file)
  
  if (!is.null(file))
    assert(es$storyId == str.left.of(basename(file),"."))
  
  assert(!is.null(es$scenario$params$T))
  # shocks must be part of the scenario
  assert(is.null(es$shocks))
  # T must be part of the scenario$T
  assert(is.null(es$T))
  
  
}


check.model = function(em) {
  restore.point("check.model")
  
  # all known symbols
  syms = c(names(em$vars),names(em$params), names(em$extraVars))
  li = lapply(em$scenarios, function(scen) names(scen$params))
  syms = unique(c(syms, unlist(li)))
  syms = c(syms, paste0("lag_",syms),paste0("lead_",syms),"t")
  
  check.formula = function(obj,field="formula",name=get.name(obj),section="", allow=NULL) {
    if (length(field)>1) {
      code = paste0("obj", paste0("[['",field,"']]",collapse=""))
      val = eval(parse(text=code))
    } else {
      val = obj[[field]]
    }
    
    if (length(val)==0) return(TRUE)
    vars = find.variables(parse.as.call(val))
    unknown = setdiff(vars,c(syms,allow))
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
  
  lapply(em$curves, function(cu) {
    check.formula(cu, section="curves", field="eq", allow=cu$xy)
  })
  invisible(TRUE)
  
}
