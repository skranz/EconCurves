library(ggplot2)


myplot = function(sim, x="t",y="R", color="scenario",fill=color, alpha=0.7, size=1.5, title=NULL, ...) {
  restore.point("myplot")
  p = ggplot(data=sim, aes_string(x=x,y=y,color=color,fill=fill)) + geom_line(size=1.5,alpha=alpha)
  if (!is.null(title))
    p = p + ggtitle(title)
  p
}

compute.extra = function(sim,...) {
  library(tidyr)
  library(dplyrExtras)
  d = dplyr::mutate(sim,Rtr = Rtr*alpha,Rout=Rout*(1-alpha))
  
  d = dplyr::select(sim,t,scenario, Treaty_Countries=Rtr,None_Treaty_Countries=Rout)
  d = gather(d,key = "country", value="R", 3:4)
  d = arrange(d, scenario, country,t)
  d = mutate(group_by(d,scenario,country), cumR=cumsum(R))
  
  ds = summarise(group_by(d, scenario, country), R_total=sum(R))
  
  list(d = d, ds=ds)
}

plot_vs_timeline = function(sim,var="R",title="Treaty- vs Non-Treaty-Countries",...) {
  li = compute.extra(sim)
  d = li$d; ds = li$ds

  ggplot(data=d, aes_string(y=var, x="t", group="scenario", color="scenario")) + geom_line(size=1.3, alpha=0.7) + facet_wrap(~country) + ggtitle(title)
  
}

plot_vs_barplot = function(sim,var="R_total",title="Treaty- vs Non-Treaty-Countries",...) {
  li = compute.extra(sim)
  d = li$d; ds = li$ds

  ggplot(data=ds, aes_string(y=var, x="country", fill="country")) + geom_bar(stat = "identity") + facet_wrap(~scenario) + ggtitle(title)
  
}
