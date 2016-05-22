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
  
  show.colors(c(greens,cyans, blues, lilas, purples, reds, oranges, yellows),n)
}

color.pals = function() {
  list(
    black = c("#111111","#999999"),
    grey = c("#111111","#999999"),
    #blue = c("blue","#1F78B4", "#A6CEE3"),
    blue = c("blue","#6688ff", "#A6CEE3"),
    #green = c("green","#33A02C", "#B2DF8A"),
    green = c("green", "#00dd00","#77ff77", "#B2DF8A"),
    #red = c ("red","#E31A1C", "#FF9A99"),
    red = c("#ff0000","#ff7777", "#FF9A99"),
    orange = c("#FF7F00","#FDBF6F"),
    lila = c("#6A3D9A","#AA92B6"),
    yellow = c("yellow","#FFDF80"),
    brown = c("brown","#B3430F"),
    purple = c("#B30059","#FF80BF"),
    cyan = c("#00A3A3","#8FCCCC")
  )
}

curve.color = function(base="blue",level=1,color=NULL) {
  restore.point("curve.color")
  
  if (!is.null(color))
    return(color)
  
  if (is.null(base))
    return("black")
  library(RColorBrewer)
  
  pal = color.pals()  
  if (!base %in% names(pal)) base = "grey"
  
  li = pal[[base]]
  if (level > length(li)) return(grey(0.8))
  li[level]
} 

show.colors = function(colors, colCount=ceiling(sqrt(length(colors)))) {
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
