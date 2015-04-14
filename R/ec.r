ec = new.env()


init.ec = function(path=getwd(),
    models.path = paste0(path,"/models"),
    types.path = paste0(path,"/yamltypes")
) {
  restore.point("init.ee")
  options(stringsAsFactors = FALSE)
  ec$main.path = path
  ec$types.path = types.path
  ec$models.path = models.path
  yaml.objects.settings(default.types.path=types.path)
  ec$types = load.types()
}

get.ec = function() {
  ec
}

examples.load.model = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  res = load.model("ThreeEq")
  tt = res$tt
  em = res$em
}

load.model = function(modelId, file=paste0(modelId,".yaml"), dir=get.ec()$models.path, ec = get.ec()) {
  restore.point("load.model")
  
  tt = load.struct(name="model",file = paste0(dir,"/",file),typeName = "model")
  list(em = tt.object(tt,1), tt=tt)
  
}