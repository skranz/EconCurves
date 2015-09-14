


load.story = function(storyId, file=paste0(storyId,".yaml"), dir=get.ec()$stories.path, text=NULL, ec = get.ec()) {
  restore.point("load.story")

  tt = load.struct(name="story",file = paste0(dir,"/",file),typeName = "story",text=text)
  
  obj = tt.object(tt,1)
  parts = setdiff(names(obj),c("storyId","settings"))
  
  es = as.environment(c(
    list(storyId = as.character(obj$storyId)), 
    obj$settings,
    list(parts = obj[parts])
  ))
  
  #es = as.environment(tt.object(tt,1))
  #es = as.environment(read.yaml(file=paste0(dir,"/",file)))
  
  es$yaml = attr(tt,"yaml")
  Encoding(es$yaml) <- "UTF-8"

  check.story(file = file, es=es)
  
  es
}

init.story = function(es, em=NULL) {
  restore.point("init.story")
  
  if (is.null(em))
    em = load.model(es$modelId)
  es$em = em
  init.model(em = es$em)
  
  if (is.null(es[["storyType"]])) es$storyType = "dynamics"
  
  if (es$storyType=="dynamics") {
    init.dynry(es,em=em)
  } else if (es$storyType=="scenarios") {
    init.scenry(es,em=em)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
  
}

story.ui = function(app=getApp(), es=app$es) {
  restore.point("story.ui")
  if (es$storyType=="dynamics") {
    ui = dynry.ui(app=app, es=es)
  } else if (es$storyType=="scenarios") {
    # No need to run if ui is shown
    ui = scenry.ui(app = app)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
  ui
}

run.story = function(es) {
  restore.point("run.story")
  if (es$storyType=="dynamics") {
    dynry.tell.part.task()
  } else if (es$storyType=="scenarios") {
    # No need to run if ui is shown
    scenry.show.part(part.num = 1, init.part=TRUE)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
}


get.story.part.task.type = function(part) {
  restore.point("get.story.step.task.type")
  types = c("find","shift","select","findPoint")
  not.null = which(sapply(types, function(type) {
    !is.null(part$task[[type]])
  }))
  if (length(not.null)==0) return("unknown")
  
  types[not.null]
}


story.part.task.symbols = function(part) {
  restore.point("step.task.symbols")
  tasks = setdiff(names(part$task),c("pane","type") )
  symbols = unique(unlist(lapply(part$task[tasks], function(ta) ta$symbol)))
  symbols
}
