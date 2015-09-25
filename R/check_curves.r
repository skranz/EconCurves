missing.fields = function(li, fields) {
  missing = setdiff(fields, names(li))
  return(missing)
}

check.curve = function(curve) {
  restore.point("check.curve")

  missing = missing.fields(curve, c("xvar","yvar","name","eq","color"))
  if (length(missing)>0) {
    stop(paste0("You curve has not specified the required fields ", paste0(missing, collapse=", "),"."))
  }

}
