ee = new.env()


init.ee = function(path=getwd(),
    struc.path = paste0(path,"/Structures"),
    types.path = paste0(path,"/Types"),
    data.path = paste0(path,"/Data"),
    cache.path = paste0(path,"/Cache"), 
    gambit.path = paste0(path,"/gambit") # by gambit should be added to package or be on system path
) {
  restore.point("init.ee")
  options(stringsAsFactors = FALSE)
  ee$main.path = path
  ee$struc.path = struc.path
  ee$types.path = types.path
  ee$data.path = data.path
  ee$cache.path = cache.path
  ee$game.struct = list()
  ee$exp.struct = list()
  
  yaml.objects.settings(default.types.path=types.path)
  ee$types = load.types()
  
  ee$expId = NULL
  ee$gameId = NULL
  
  ee$gambit.bin.path = paste0(gambit.path,"/bin")
  ee$gambit.efg.path = paste0(gambit.path,"/efg")
  
  
  ee$sfg = list()
}

get.ee = function() {
  ee
}