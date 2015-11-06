check.geoms.beside = function(old, new) {
  restore.point("check.geom.beside")
  
  if (old$type != new$type) {
    return(ok=NA, ydir = NA, xdir=NA)
  }
  geom.type = old$geom.type
  if (geom.type=="line") {
    return(check.lines.beside(old, new))
  }
  return(ok=NA, ydir = NA, xdir=NA)

}

line.slopes = function(line) {
  diff(line$y) / diff(line$x)
}

check.lines.beside = function(old, new) {
  restore.point("check.lines.beside")

  old.slopes = line.slopes(old)
  new.slopes = line.slopes(new)
  
  # all points of new a right of old
  if (min(new$x) > max(old$x)) {
    xdir = 1
    if (all(old.slopes<0) & all(new.slopes<0)) {
      ydir = 1
    } (all(old.slopes>0) & all(new.slopes>0)) {
      ydir = -1
    } else  {
      ydir = 0
    }
    return(ok=TRUE, )
  }
      
  nr = length(old$y)
  nc = length(new$y)
  

  oymat = matrix(old$y, nrow=nr,ncol=nc)
  nymat = matrix(new$y, nrow=nr,ncol=nc, byrow=TRUE)

  oxmat = matrix(old$y, nrow=nr,ncol=nc)
  nxmat = matrix(new$y, nrow=nr,ncol=nc, byrow=TRUE)

    
  ref.shift = line.to.line.shift(line, ref.line)

  point.shift = sign(point.to.line.pos(xy,ref.line))
  if (all(point.shift==ref.shift))
    return(TRUE)
  return(FALSE)

}
