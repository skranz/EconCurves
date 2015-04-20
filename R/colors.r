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

color.table = function() {
#   n = 2
#   yellows = c(hue.palette(n, h=1/8)[2],"yellow")
#   purples = rev(hue.palette(n, h=11/12))
#   cyans = c(hsv(h=1/2,s=0.25,v=1),hsv(h=1/2,s=1,v=0.7))
#   browns = c("orange3",rev(hue.palette(n, h=1/16))[2])
#   
#   colors = c(brewer.pal(n=12, "Paired")[1:10],
#     yellows, browns, purples, cyans)
#   
#   cat(paste0('"',colors,'"', collapse=","))
  colors = c("#999999","#111111", "#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FF9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFDF80FF","yellow","orange3","#B34300FF","#FF80BFFF","#B30059FF","#BFFFFF","#00B3B3")
  
  #show.colors(colors,2)
  
  df = data.frame(
    color=colors,
    base = c("black","black", "blue","blue","green","green","red","red",
      "orange","orange","lila","lila","yellow","yellow",
      "brown","brown","purple","purple","cyan","cyan"),
    level = c(2,1)
  )
  df
}

curve.color = function(base="blue",level=1,color=NULL) {
  restore.point("curve.color")
  
  if (!is.null(color))
    return(color)
  
  if (is.null(base))
    return("black")
  library(RColorBrewer)
  
  color.table = color.table()
  
  
  if (base=="grey") {
    return(grey(0.5))
  }
  
  sel.df = data_frame(base=base, level=level)
  df = left_join(sel.df, color.table, by=c("base","level"))
  df$color[is.na(df$color)] = "black"
  df$color
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
