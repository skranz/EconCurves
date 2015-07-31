deparse1 = function (call, collapse = "") 
{
    paste0(deparse(call, width = 500), collapse = collapse)
}


subst.var = function(call, var, subs, subset=FALSE) {
  restore.point("substitute.variable")
  if (!is.character(var)) var = deparse(var)
  if (is.character(call)) call = parse(text=call)[[1]]
  if (is.character(subs)) {
    subs = lapply(subs, function(s) {
      parse(text=s)[[1]]
    })
  }
  if (!is.list(subs)) {
    sub.li = list(subs)
  } else {
    sub.li = subs
  }
      
  names(sub.li) = var
  
  res = substitute.call(call, sub.li)
  #if (subset) res = res[[1]]
  res
}


bound.value = function(x, lower=NULL, upper=NULL) {
  if (!is.null(lower)) {
    x[x<lower] = lower
  }
  if (!is.null(upper)) {
    x[x>upper] = upper
  }
  x
}

