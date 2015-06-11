green.paradox = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("GreenParadox")
  
  init.model(em)
  init.model.scen(em)
  
  scela = list(
    base.scen = em$scen,
    scens = list(
      no_treaty = list(alpha=0),
      treaty = list(alpha=0.7, start_q_tr=50, shrink=0.2, start_t=8)
    ),
    adapt.scen.sim = function(sim) {
      sim$burned = cumsum(R)
      sim
    },
    plots = list(
      R = function(sim,...) {
        ggplot(data=sim, aes(x=t,y=R,color=.scen,fill=.scen)) + geom_line(size=1.5,alpha=.7)
      },
      burned = function(sim,...) {
        ggplot(data=sim, aes(x=t,y=burned,color=.scen,fill=.scen)) + geom_line(size=1.2)
      }
    )
  )
  scela

  
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


scenry.ui = function(app=getApp(), scela) {
  restore.point("scela.ui")
  ui = fluidRow(
    
    column(8,
      story.panes.ui(),
      dygraphOutput("dygraph",height="150px")
      #uiOutput("panesUI")
    )
  ) 
  #setPlot("testPlot",{plot(runif(100),runif(100))})
  #setUI(id = "panesUI",story.panes.ui())
  buttonHandler("stNextBtn", story.next.btn.click)
  buttonHandler("stForwardBtn", story.forward.btn.click)
  buttonHandler("stPrevBtn", story.prev.btn.click)
  buttonHandler("stExitBtn", exit.to.main)
  

  ui
}
