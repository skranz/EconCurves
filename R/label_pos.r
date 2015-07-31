
get.endpoints = function(lines) {
  restore.point("get.endpoints")
  #line = lines[[2]]
  li = lapply(lines, function(line) {
    restore.point("uhsfanjadnfjn")
    n = min(length(line$x), length(line$y))
    if (n==0) return(NULL)
    if (n==1) {
      res = data_frame(x=line$x[1],y=line$y[1],line=line$name)
    } else {
      res = data_frame(x=line$x[c(1,n)],y=line$y[c(1,n)],line=line$name)
    }
    res
  })   
  bind_rows(li)
}

find.label.pos = function(lines, yrange,yshift=diff(yrange)*0.05, do.shuffle=FALSE) {
  restore.point("find.label.pos")    
  linas = sapply(lines, function(line) line$name)
  
  ep.df = get.endpoints(lines)
  ep.df$remain = TRUE

  if (do.shuffle) {
    shuffle = sample.int(length(linas))
  } else {
    shuffle = seq_along(linas)
  }
  i = 1
  # greedy search: find end points that are closest
  for (i in shuffle) {
    cuna =linas[i]
    rows = which(ep.df$line == cuna)
    ep.df$remain[rows] = FALSE
    dist = sapply(rows,ep.df=ep.df, function(row, ep.df) {
      x = ep.df$x[row]; y = ep.df$y[row]
      dist = min( (ep.df$x[ep.df$remain]-x)^2 + (ep.df$y[ep.df$remain]-y)^2)
      dist
    })
    sel.row = rows[which.max(dist)]
    ep.df$remain[sel.row] = TRUE
  }
  
  label.pos = ep.df[ep.df$remain, 1:3]
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