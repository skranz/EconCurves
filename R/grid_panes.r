create.panes.viewports = function(panes) {
  panes = em$panes
  pos.df = bind_rows(lapply(panes, function(pane) data_frame(x=pane$pos[1],y=pane$pos[2])))
  
  
  grid.newpage()
testlay <- function(just="centre") {
  pushViewport(viewport(layout=grid.layout(1, 1, widths=unit(1, "inches"),
                          heights=unit(0.25, "npc"),
                          just=just)))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.rect()
  grid.text(paste(just, collapse="-"))
  popViewport(2)
}
testlay()
testlay(c("left", "top"))

} 