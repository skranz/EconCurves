examples.solve_symb = function() {
  cond = quote((sum(R) == Smax) | ((sum(R) <= Smax) & all(m==0))) 
  pryr:::call_tree(cond)
  res = change.nested.call(cond,adapt.call=adapt.to.implicit.eq)
  res
    
  .u = 5
  pattern = quote(.x + .y == . )
  new = quote(.x == . -(.y))
  call = quote((1+3+x)+z == 5)
  
  li = match.pattern(call, pattern)
  
  substitute.call(new,li)
  
  
  cond = quote(.a*.x + .x)
  pryr:::call_tree(cond)
  
inv.rules.txt = 
'
log(.x) == . <=> .x == exp(.)
.x + .y == . <=> .x == . - (.y)
.y + .x == . <=> .x == . - (.y)
.x - .y == . <=> .x == . + .y
.y - .x == . <=> .x == -(.) + .y
.x * .y == . <=> .x == . / .y
.y * .x == . <=> .x == . / .y
'
inv.rules = parse.equation.rules(inv.rules.txt)



collect_rules = 
'
.x + .x      --> 2*.x
.a*.x + .x    --> (.a+1)*.x
.x + .b*.x    --> (.b+1)*.x
.a*.x + .b*.x --> (.a+.b)*.x
'
}

parse.equation.rules = function(text) {
  text = inversion_rules
  library(stringtools)
  
  text = sep.lines(text)
  text = str.trim(text)
  rows = !str.starts.with(text,"#") & has.substr(text,"<=>")
  text = text[rows]
  
  old.txt = str.left.of(text,"<=>")
  new.txt = str.left.of(text,"<=>")
  
  old = as.list(parse(text=old.txt))
  new = as.list(parse(text=new.txt))
  
  old.lhs = lapply(old, function(eq) eq[[2]])
  old.rhs = lapply(old, function(eq) eq[[3]])

  new.lhs = lapply(new, function(eq) eq[[2]])
  new.rhs = lapply(new, function(eq) eq[[3]])

  
  nlist(old.lhs, old.rhs, new.lhs, new.rhs)
      
} 

identical.symbol = function(call, symbol) {
  identical(call, symbol)  
}

match.pattern = function(call, pattern, check.identical=TRUE) {
  restore.point("match.pattern")
  
  if (is.name(pattern)) {
    pname = as.character(pattern)
    if (str.starts.with(pname,".")) {
      res = list(call)
      names(res) = pname
      return(res)
    }
    return(list())
  }
  
  if (is.atomic(call) | is.name(call)) return(NA)
  pname = pattern[[1]]
  cname = call[[1]]
  if (!identical(pname,cname)) return(NA)
  
  if (length(call) != length(pattern)) return(NA)
  
  restore.point("match.pattern.2")

  inds = seq.int(2,length(pattern))
  li = vector("list",length(inds))
  for (i in inds) {
    res = match.pattern(call[[i]], pattern[[i]], check.identical=FALSE)
    if (identical(res,NA)) return(NA)
    li[[i]] = res
  }
  li = do.call(c,li)
  
  # make sure that 
  if (check.identical) {
    
  }
  li
}

isolate.symbol = function(eq,sym, inv.rules) {
  restore.point("isolate.symbol")
  
  
  
    
}

collect.symbol = function(eq,sym, collect.rules) {
  
}

