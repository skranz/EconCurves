
get.endpoints = function(gcurves) {
  restore.point("get.endpoints")
  #gcurve = gcurves[[2]]
  li = lapply(gcurves, function(gcurve) {
    restore.point("uhsfanjadnfjn")
    n = min(length(gcurve$x), length(gcurve$y))
    if (n==0) return(NULL)
    if (n==1) {
      res = quick.df(x=gcurve$x[1],y=gcurve$y[1],
        gcurve=gcurve$name, label=gcurve[["label"]])
    } else {
      res = quick.df(x=gcurve$x[c(1,n)],y=gcurve$y[c(1,n)],
        gcurve=rep(gcurve$name,2), label=rep(gcurve$label,2))
    }
    res
  })   
  as_data_frame(bind_rows(li))
}

find.label.pos = function(gcurves, yrange,yshift=diff(yrange)*0.05, do.shuffle=FALSE) {
  restore.point("find.label.pos")    
  linas = sapply(gcurves, function(gcurve) gcurve$name)
  
  ep.df = get.endpoints(gcurves)
  ep.df$remain = TRUE

  
  # For a single gcurve pick the last point
  if (length(linas)==1) {
    ep.df$remain = FALSE
    ep.df$remain[NROW(ep.df)] = TRUE
  
  # For multiple gcurves try to find endpoint that is farthest away
  # from other endpoints
  } else {
    if (do.shuffle) {
      shuffle = sample.int(length(linas))
    } else {
      shuffle = seq_along(linas)
    }
    i = 1

    # greedy search: find end points that are closest
    for (i in shuffle) {
      cuna =linas[i]
      rows = which(ep.df$gcurve == cuna)
      ep.df$remain[rows] = FALSE
      dist = sapply(rows,ep.df=ep.df, function(row, ep.df) {
        x = ep.df$x[row]; y = ep.df$y[row]
        dist = min( (ep.df$x[ep.df$remain]-x)^2 + (ep.df$y[ep.df$remain]-y)^2)
        dist
      })
      sel.row = rows[which.max(dist)]
      ep.df$remain[sel.row] = TRUE
    }
  }  
  label.pos = ep.df[ep.df$remain, 1:4]
  dupl = which(duplicated(label.pos[,c("x","y")]))
  sign = -((-1)^(seq_along(dupl)))
  label.pos$y[dupl] = label.pos$y[dupl]+sign*yshift 
  label.pos
}

# copied from package TeachingDemos from Greg Snow
shadowtext = function(x, y=NULL, labels, col='white', bg='black',
	theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {

	xy <- xy.coords(x,y)
	xo <- r*strwidth('A')
	yo <- r*strheight('A')
	for (i in theta) {
		text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
	}
	text(xy$x, xy$y, labels, col=col, ... )
}