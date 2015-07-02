.safe_env <- new.env(parent = emptyenv())

init_safe_env = function() {
  safe_f <- c(
    getGroupMembers("Math"),
    getGroupMembers("Arith"),
    getGroupMembers("Compare"),
    "<-", "{", "(","min","max","pmin","pmax",
    "seq",":","seq.default","seq.int"
  )
  
  for (f in safe_f) {
    .safe_env[[f]] <<- get(f, "package:base")
  }
}

safe_eval <- function(call) {
  if (is.null(.safe_env$min)) init_safe_env()
  eval(call, env=.safe_env)
}

safe_parse_eval_number <- function(text) {
  if (length(text)==0) return(text)
  #if (is.numeric(text)) return(text)
  
  num = suppressWarnings(as.numeric(text))
  if (!is.na(num)) return(num)
  
  call = parse(text=text)
  if (is.null(.safe_env$min)) init_safe_env()
  res = eval(call, env=.safe_env)
  if (!is.numeric(res))
    stop("safe_eval_number did not return a number...")
  return(res)
}
