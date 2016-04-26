example.curly.braces = function() {
  library(pBrackets)
  par(mar=c(1,1,1,1))
  plot(0,0, type='n', xlim=c(0,20), ylim=c(0,20), axes=FALSE, xlab='', ylab='')
  abline(h=seq(0,20), v=seq(0, 7), col=rgb(0.8, 0.9, 0.95))
  brackets(0, 18, 7, 20, lwd=2)
  text(8, 20, labels=expression(paste(bold('Braces:'), ' default')), adj=c(0,0))
  brackets(0, 16, 7, 18, lwd=2, curvatur=1, type=2)
  text(8, 18, labels=expression(paste(bold('Braces 2:'), ' curvatur=1, type=2')), adj=c(0,0))

}

