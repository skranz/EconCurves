check.story = function(file=NULL, es=NULL) {
  restore.point("check.story")
  
  if (is.null(es))
    es <- load.story(file = file)
  
  if (!is.null(file))
    assert(es$storyId == str.left.of(basename(file),"."))
  
  assert(!is.null(es$scenario$params$T))
  # shocks must be part of the scenario
  assert(is.null(es$shocks))
  # T must be part of the scenario$T
  assert(is.null(es$T))
  
  
}