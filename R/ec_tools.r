copy.into.nested.list = function(given, new, return.null.new=FALSE) {
  restore.point("copy.into.nested.list")
  
  if (length(new)==0) {
    if(return.null.new) return(new) 
    return(given)
  }
  names = names(new)
  if (is.null(names)) return(new)
  if (is.list(given)) {
    is.li = sapply(given[names], is.list)
    given[names[!is.li]] = new[!is.li]
  } else {
    is.li = sapply(names, function(name) is.list(given[[name]]))
    for (name in names[!is.li]) {
      given[[name]] = new[[name]]  
    }
  }
  for (name in names[is.li]) {
    given[[name]] = copy.into.nested.list(given[[name]],new[[name]], return.null.new=TRUE)
  }
  given
}

compute_frame = function(..., parent.env = parent.frame()) {
  ali = eval(substitute(alist(...)))
  restore.point("compute_frame")
  
  fun.env = new.env(parent=parent.env)
  ..env = new.env(parent=fun.env)
  
  i = 1
  fun = function(...) {}  
  environment(fun) = ..env
  
  for (i in seq_along(ali)) {
    rhs = ali[[i]] 
    name = names(ali)[i]
    call = substitute(val <- rhs, list(val= as.name(name), rhs=rhs))
    try(eval(call, ..env))

    body = substitute({
      ..vli = list(...)
      eval(quote(rhs),envir = ..vli)
    }, list(rhs=rhs,name=name))
    body(fun) <- body
    fun.env[[name]] = fun
  }
  li = as.list(..env)[names(ali)]
  do.call(data_frame,li)
}

is.false = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = TRUE  
  return(!val)
}

random.string = function(n=1,nchar=14, set=c(letters,LETTERS,0:9)) {
  chars = sample(set,nchar*n, replace = TRUE)
  if (n == 1) return(paste0(chars, collapse=""))
  mat = as.data.frame(matrix(chars, n, nchar))
  do.call(paste0,mat)
}

copy.into.null.fields = function(dest, source) {
  restore.point("copy.into.fields")
  
  snames = names(source)
  dest.val = dest[snames]
  dest.null = sapply(dest.val, is.null)
  
  dest[snames[dest.null]] = source[dest.null]
  dest
}


replace.whiskers = function(str, env=parent.frame()) {
  restore.point("replace.whiskers")
  
  if (is.null(str)) return(str)
  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])
  vals = lapply(s, function(su) {
    res = try(eval(parse(text=su),env))
    if (is(res,"try-error")) res = "`Error`"
    res
  })
  res = str.replace.at.pos(str, pos$outer, unlist(vals))
  res
}


compile.story.txt = function(txt, out="text",val =as.list(em$sim[t,,drop=FALSE]),  em=NULL,t=1, digits=4) {
  restore.point("compile.story.txt")
  
  if (length(txt)==0) return("")
  
  val = lapply(val, function(v) {
    if (is.numeric(v)) return(signif(v,digits))
    return(v)
  }) 
  
  txt = replace.whiskers(paste0(txt, collapse="\n"), val)

  if (out=="text") {
    txt = gsub("$","",txt, fixed=TRUE)
  } else if (out=="html") {
    restore.point("compile.story.txt.2")
    txt = markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)
    #Encoding(txt) <- "UTF-8"
    txt
  }
  txt
  
}



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

