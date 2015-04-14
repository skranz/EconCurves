hue.palette = function(n, h = 0, s.start = 1, s.end=0.35, v.start = 0.4, v.end=1, alpha=1) {
  if (n<=2) {
    s = c(1,  0.5)[1:n]
    v = c(0.7,1)[1:n]  
  } else if (n<=3) {
    s = c(1,0.5,0.9)[1:n]
    v = c(0.9,1,0.6)[1:n]
  } else {
    s = seq(s.start,s.end, length=n)
    v = seq(v.start,v.end, length=n)
  }
  hsv(h=rep(h,n),alpha=alpha,
      s=s,
      v=v)
}

reds = function(n, alpha=1, h=0) {
  if (n<=2) {
    s = c(1,  0.6)[1:n]
    v = c(0.9,1)[1:n]  
  } else if (n<=3) {
    s = c(1,0.5,0.9)[1:n]
    v = c(0.9,1,0.6)[1:n]
  } else {
    return(hue.palette(n, h=0, alpha=alpha))
  }
  hsv(h=rep(0,n),alpha=alpha,s=s, v=v)
}


blues = function(n, alpha=1, h=2/3) {
  if (n<=2) {
    s = c(1,  0.3)[1:n]
    v = c(0.7,1)[1:n]  
  } else if (n<=3) {
    s = c(1,0.5,0.9)[1:n]
    v = c(0.9,1,0.6)[1:n]
  } else {
    return(hue.palette(n, h=h, alpha=alpha))
  }
  hsv(h=rep(h,n),alpha=alpha,s=s, v=v)
}


examples.hue.palette = function() {
  n = 2
  reds = reds(n)
  oranges = hue.palette(n, h=1/16)
  yellows = hue.palette(n, h=1/8)
  greens = hue.palette(n, h=0.25)
  blues = blues(n)
  lilas = hue.palette(n, h=9/12)
  cyans = hue.palette(n, h=1/2)
  purples = hue.palette(n, h=11/12)
  
  color.table(c(greens,cyans, blues, lilas, purples, reds, oranges, yellows),n)
}

color.table = function(colors, colCount=ceiling(sqrt(length(colors)))) {
  n = length(colors)
  rowCount = ceiling(n / colCount)

  plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
    axes=FALSE, ylim=c(rowCount,0))
  title("Colors")

  for (j in 0:(rowCount-1))
  {
    base <- j*colCount
    remaining <- length(colors) - base
    RowSize <- ifelse(remaining < colCount, remaining, colCount)
    rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
      border="black",
      col=colors[base + (1:RowSize)])
    text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
      col="black")
  }
}
