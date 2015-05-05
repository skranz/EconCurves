#setwd("D:/libraries/EconCurves/EconCurves/app")

library(EconCurves)
init.ec()
ec = get.ec()
load.collection("makro.yaml")

app = shinyStoriesApp(ec = ec)
app$verbose = FALSE
app$is.running = TRUE

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app,launch.browser = rstudio::viewer)
