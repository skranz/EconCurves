
get.endpoints = function(geoms) {
  restore.point("get.endpoints")
  #geom = geoms[[2]]
  li = lapply(seq_along(geoms), function(ind) {
    restore.point("uhsfanjadnfjn")
    geom = geoms[[ind]]
    n = min(length(geom$x), length(geom$y))
    if (n==0) return(NULL)
    if (n==1) {
      res = quick.df(x=geom$x[1],y=geom$y[1],ind=ind)
    } else {
      res = quick.df(x=geom$x[c(1,n)],y=geom$y[c(1,n)],ind=rep(ind,2))
    }
    res
  })   
  as_data_frame(bind_rows(li))
}

find.label.pos = function(geoms, yrange,yshift=diff(yrange)*0.05, do.shuffle=FALSE) {
  restore.point("find.label.pos")  

  inds = seq_along(geoms)

  ep.df = get.endpoints(geoms)
  ep.df$remain = TRUE

  
  # For a single geom pick the last point
  if (length(inds)==1) {
    ep.df$remain = FALSE
    ep.df$remain[NROW(ep.df)] = TRUE
  
  # For multiple geoms try to find endpoint that is farthest away
  # from other endpoints
  } else {
    if (do.shuffle) {
      shuffle = sample.int(length(inds))
    } else {
      shuffle = inds
    }
    i = 1

    # greedy search: find end points that are closest
    for (i in shuffle) {
      ind =inds[i]
      rows = which(ep.df$ind == ind)
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
