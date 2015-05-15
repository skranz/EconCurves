examples.solve_symb = function() {
  .u = 5
  pattern = quote(.x + .x == . )
  new = quote(.x == (.)/2)
  call = quote((1+3+4*x)+z == 5)
  call = quote(z+z == 5)
  
  isolate.symbol(call,sym = quote(x),)
  
  li = match.pattern(call, pattern)
  
  substitute.call(new,li)

inv.rules.txt = 
'
(.x)    == . <=> .x == .
log(.x) == . <=> .x == exp(.)
.x + .y == . <=> .x == . - (.y)
.y + .x == . <=> .x == . - (.y)
.x - .y == . <=> .x == . + .y
.y - .x == . <=> .x == -(.) + .y
.x * .y == . <=> .x == (.) / (.y)
.y * .x == . <=> .x == (.) / (.y)
sqrt(.x) ==. <=> .x == sqr(.)
#.x^.y == .  <=> .x == (.)^(1/(.y)) 
'
  inv.rules = parse.equation.rules(inv.rules.txt)
  eq = quote((1+3+4*x^2)+z == 5)
  isolate.symbol(eq, sym=quote(x),inv.rules)

collect_rules = 
'
.x + .x       --> 2*.x
.a*.x + .x    --> (.a+1)*.x
.x + .b*.x    --> (.b+1)*.x
.a*.x + .b*.x --> (.a+.b)*.x
.x - .x       --> 0
.a*.x - .x    --> (.a-1)*.x
.x - .b*.x    --> (.b-1)*.x
.a*.x - .b*.x --> (.a-.b)*.x
'

}

parse.equation.rules = function(text) {
  parse.rules(text, sep="<=>")
} 

parse.term.rules = function(text) {
  parse.rules(text, sep="-->")
} 


parse.rules = function(text, sep="<=>") {
  restore.point("parse.ruls")
  library(stringtools)
  
  text = sep.lines(text)
  text = str.trim(text)
  rows = !str.starts.with(text,"#") & has.substr(text,sep)
  text = text[rows]
  
  old.txt = str.left.of(text,sep)
  new.txt = str.right.of(text,sep)
  
  old = as.list(parse(text=old.txt))
  new = as.list(parse(text=new.txt))
  
  nlist(old, new)
} 


identical.symbol = function(call, symbol) {
  identical(call, symbol)  
}

contains.symbol = function(call, symbol) {
  if (identical(call,symbol)) return(TRUE)
  if (length(call)==1) return(FALSE)
  
  restore.point("contains.symbol")
 
  for (i in seq_along(call)) {
    if (contains.symbol(call[[i]], symbol)) return(TRUE)
  }
  
  return(FALSE)
}


match.pattern = function(call, pattern, check.identical=TRUE, contain.symbol=NULL) {
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
  
  # make sure that identical placeholders are in fact identical
  if (check.identical) {
    if (!are.same.placeholders.identical(li))
      return(NA)
    li = li[unique(names(li))]
  }
  for (i in seq_along(li)) {
    if (!are.symbols.contained(li[[i]],names(li)[i],contain.symbol))
      return(NA)
  }
  
  li
}

are.symbols.contained = function(call, placeholder, contain.symbol) {
  restore.point("are.symbols.contained")
  cs = contain.symbol[[placeholder]]
  if (length(cs)>0) {
    return(contains.symbol(call = call,symbol = cs))
  }
  return(TRUE)
}

are.same.placeholders.identical = function(li) {
  restore.point("are.same.vars.identical")
  names = names(li)
  vars = names[duplicated(names)]
  for (var in vars) {
    inds = which(names == var)
    first = inds[1]
    for (i in inds[-1]) {
      if (!identical(li[[first]],li[[i]]))
        return(FALSE)
    }
  }
  return(TRUE)
}

isolate.symbol = function(eq,symbol, inv.rules) {
  restore.point("isolate.symbol")
  
  eq = substitute((lhs - (rhs)) == 0, list(lhs=eq[[2]],rhs=eq[[3]]))  

  old.eq = eq
  
  ok = FALSE
  i = 1
  contain.symbol.li = list(.x=symbol)
  while(TRUE) {
    for (i in seq_along(inv.rules$old)) {
      pattern = inv.rules$old[[i]]
      m = match.pattern(eq,pattern,contain.symbol = contain.symbol.li) 
      if (!identical(m,NA)) {
        eq = substitute.call(inv.rules$new[[i]], m)
        if (identical(eq[[2]],symbol)) {
          ok = TRUE
          break
        }
      }
    }
    if (ok) break
    if (identical(eq,old.eq)) break
    old.eq = eq
  }
  list(solved=ok, eq=eq)
}

collect.symbol = function(eq,sym, collect.rules) {
  
}

