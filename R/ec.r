ec = new.env()

# check the package for bugs
check.econ.curves = function() {
  txt = NULL
  codetools::checkUsagePackage("EconCurves",
    report = function(str)  txt <<- c(txt,str),
    suppressLocalUnused = TRUE  
  ) 
  txt
  rows = str.starts.with(txt,"examples.")
  rows = rows | has.substr(txt,"no visible global function definition")
  txt = txt[!rows]
  cat(paste0(1:NROW(txt),": ", txt, collapse="\n"))
  invisible(txt)
}

init.ec = function(path=getwd(),
    models.path = paste0(path,"/models"),
    stories.path = paste0(path,"/stories"),
    types.path = paste0(path,"/yamltypes"),
    allow.edit=TRUE
) {
  restore.point("init.ee")
  options(stringsAsFactors = FALSE)
  ec$main.path = path
  ec$types.path = types.path
  ec$models.path = models.path
  ec$stories.path = stories.path
  yaml.objects.settings(default.types.path=types.path)
  ec$types = load.types()
  ec$allow.edit = allow.edit
  ec
}

get.ec = function() {
  app = try(getApp(),silent=TRUE)
  if (!is(app,"try-error")) {
    if (!is.null(app[["ec"]]))
      return(app[["ec"]])
  }
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
  list(em = as.environment(tt.object(tt,1)), tt=tt)
  
}