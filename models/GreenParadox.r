
addCumR = function(sim) {
  sim$cumR = cumsum(R)
  sim
}

plotR = function(sim,...) {
  ggplot(data=sim, aes(x=t,y=R,color=.scen,fill=.scen)) + geom_line(size=1.5,alpha=.7)
}  

plotCumR = function(sim,...) {
  ggplot(data=sim, aes(x=t,y=cumR,color=.scen,fill=.scen)) + geom_line(size=1.2)
}

