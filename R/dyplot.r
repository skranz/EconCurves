to.xts = function (dat, time.col, interval = NULL, time = as.list(dat)[[time.col]], fill = !is.null(interval)) 
{
    restore.point("to.xts")
    drop.rows = duplicated(time) | is.na(time)
    dat = dat[!drop.rows, , drop = FALSE]
    time = time[!drop.rows]
    if (is.numeric(time)) {
      interval = "year"
      date = paste0(time,"-01-01")
      time = as.Date(date)  
    }
      
    
    if (fill) {
        full.time = seq(min(time, na.rm = TRUE), max(time, na.rm = TRUE), interval)
        dat = left_join(data.frame(.TIME = full.time), cbind(data.frame(.TIME = time), 
            dat), by = ".TIME")
        time = dat[, 1]
        dat = dat[, -1, drop = FALSE]
    }
    d = as.xts(dat, time)
    d
}

dyplot = function (data, xcol = colnames(data)[1], ycol = setdiff(colnames(data), xcol), interval = NULL,...) 
{
    restore.point("dyplot")
    library(dygraphs)
    library(xts)
    ts = to.xts(data[, ycol, drop = FALSE], time = data[[xcol]], 
        interval = interval)
    dygraph(ts,...)
}

dyplot.timelines = function(sim, cols=em$var.names, em, main="Dynamics",xlab="t",ylab="", shocks=em$sim.shocks,...) {
  
  restore.point("dyplot.timelines")
  dat = sim
  dat$t = t.to.date(dat$t)
  p = dyplot(dat, xcol="t",ycol=cols,
      xlab=xlab,ylab=ylab,main=main) %>% 
    dyOptions(stepPlot = FALSE) %>%
    #dyRangeSelector() %>%
    dyAxis("x", drawGrid = TRUE)  
  
  if (length(shocks)>0) {
    for (shock in shocks) {
      p = dy.annotate.shock(p, shock)
    }
  }
  p
}

dy.annotate.shock = function(dygraph, shock) {
  
  start = t.to.date(shock$start)
  name = shock$name
  
  p = dygraph %>% 
    dyEvent(date = start, label=name, labelLoc = "bottom")
  
  if (shock$duration>1) {
    end = t.to.date(shock$start+shock$duration-1)
    p = p%>% dyShading(from = start, to = end)
  }
  p
} 

t.to.date = function(t) {
  as.Date(paste0(t+2000,"-01-01"))
}
