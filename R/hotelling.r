green.paradox = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("GreenParadox")
  
  init.model(em)
  init.model.scen(em)
  
  Smax = 1000
  sim = simulate.scenarios(em,list(
    "no_treaty"=list(alpha=0,Smax=Smax),
    "treaty"=list(alpha=0.7, start_q_tr=50, shrink=0.2, start_t=8,Smax=Smax)
  ))

  library(ggplot2)  
  
  sim = sim %>%
    group_by(.scen) %>%
    mutate(burned = cumsum(R))
  
  ggplot(data=sim, aes(x=t,y=R,color=.scen,fill=.scen)) + geom_line(size=1.5,alpha=.7)
  ggplot(data=sim, aes(x=t,y=burned,color=.scen,fill=.scen)) + geom_line(size=1.2)
 
}

#' Simulate multiple scenarios
#' 
#' @param em the model
#' @param scen.params A list with different parameters used in the different scenarios
#' @param base.scen The basic scenario which specifies all the parameters that are not changed by scen.params
#' @param repl the number of replications for each scenario
#' @param lapply.fun the lapply function, may change e.g. to mcapply to simulate scenarioes parallely on a multicore.
simulate.scenarios = function(em, scen.params, base.scen = em$scen, repl=1, lapply.fun = lapply) {
  
  li = lapply.fun(seq_along(scen.params), function(i) {
    restore.point("simulate.scenarios.inner")
    scen.name = names(scen.params)[i]
    params = scen.params[[i]]
    scen = base.scen
    scen$params[names(params)] = params
    sim = simulate.model(em, scen=scen, init.scen=TRUE)
    data.table(.scen=scen.name,sim)
   })
  sim = as_data_frame(rbindlist(li))
  sim
}