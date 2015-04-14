
examples.grid = function() {
  library(grid)
  grid.rect(gp = gpar(lty = "dashed"))
  vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5, just = c("left", "bottom"), name = "vp1")
  vp2 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5,just = c("left", "bottom"))
  pushViewport(vp1)
  grid.rect(gp = gpar(col = "grey"))
  grid.text("Some drawing in graphics region 1", y = 0.8)
  upViewport()
  pushViewport(vp2)
  grid.rect(gp = gpar(col = "grey"))
  grid.text("Some drawing in graphics region 2", y = 0.8)
  upViewport()
  downViewport("vp1")
  grid.text("MORE drawing in graphics region 1", y = 0.2)
  popViewport()
}