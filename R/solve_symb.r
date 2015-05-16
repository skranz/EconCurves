
syso.env = new.env()


init.syso = function() {
  if (is.null(syso.env$isolate.rules))  {
    rules.txt = 
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
    syso.env$isolate.rules = parse.equation.rules(rules.txt)
    

    rules.txt = 
'
.a - .b      --> .a + -.b
(.a+.b)*.x   --> .a*.x  + .b*.x
.x * (.a+.b) --> .x*.a  + .x*.b
.a + (.b)    --> .a + .b
(.a) + .b    --> .a + .b
'
    syso.env$expand.rules = parse.term.rules(rules.txt)

      rules.txt = 
'
.x * .x      --> .x^2

.x * .x^.b   --> .x^(.b+1)
.x^.a * .x^.b   --> .x^(.a+.b)
.x^.a * .x   --> .x^(.a+1)

.a * .x      --> .x * .a
.a / .x      --> .x^-1 * .a
.x / .a      --> .x * (1/.a)
'
    syso.env$normalize.prod = parse.term.rules(rules.txt)
 
  }
}

get.syso = function() {
  init.syso()
  syso.env
}

expand.term = function(term, symb, rules = get.syso()$expand.rules ) {
  res = apply.term.rules(term, rules, repeated=TRUE)
  res
}

flatten.term = function(term, op="+") {
  if (length(term)<=1) return(list(term))
  if (term[[1]]==op) {
    li = lapply(term[-1], flatten.term, op=op)
    return(do.call(c,li))
  }
  return(list(term))
}

unflatten.term = function(term, op="+") {
  restore.point("unflatten.term")
  fun = function(op,parent,term) {
    if (length(term)==1) return(call(op,parent,term[[1]]))
    parent = call(op, parent, term[[1]])
    return(fun(op, parent, term[-1]))
  }
  if (length(term)==0) return(NULL)
  if (length(term)==1) return(term[[1]])
  res = fun(op=op,parent = term[[1]], term[-1]) 
  res
}

solve.poly = function(poly) {
  if (length(poly$bases)>1) {
    lhs.flat = lapply(seq_along(poly$bases), function(i) {
      substitute((coef)*(base), list(coef=poly$coefs[[i]], base=poly$bases[[i]]))  
    })
    lhs = unflatten.term(c(lhs.flat,list(poly$const)),"+")
    eq = substitute(lhs==0,list(lhs=lhs))
    return(list(solved=FALSE,eq=eq))
  }
  if (length(poly$base)==0) {
    return(list(solved=FALSE,eq=NULL))
  }
  lhs = poly$bases[[1]]
  rhs = substitute(const/coef, list(const=poly$const,coef=poly$coefs[[1]]))
  eq = substitute(lhs==rhs,list(lhs=lhs,rhs=rhs))
  list(solved=TRUE, eq=eq)
}

term.to.poly = function(term, var, is.expanded=FALSE) {
  restore.point("term.to.poly")
  symbol= as.symbol(var)  

  # expand term
  if (!is.expanded)
    term = expand.term(term)

  # flatten sums and then products
  terms = flatten.term(term,"+")
  terms = lapply(terms, flatten.term, op="*")

  # compute bases and coefs 
  factors.base.coef = function(factors, symbol) {
    has.symbol = sapply(factors, contains.symbol, symbol=symbol)      
    base = unflatten.term(factors[has.symbol],"*")
    coef = unflatten.term(factors[!has.symbol],"*")
    list(base=base, coef=coef)
  }
  bc.li = lapply(terms, factors.base.coef, symbol=symbol)
  bases = lapply(bc.li, function(bc) bc$base)  
  coefs = lapply(bc.li, function(bc) bc$coef)  

  # combine coefs of same bases
  ubases=unique(bases)
  
  # extract constant
  const.inds = which(sapply(bases, function(base) length(base)==0))
  if (length(const.inds)>0) {
    uconst.inds = which(sapply(ubases, function(base) length(base)==0))
    ubases = ubases[-uconst.inds]
    const = unflatten.term(coefs[const.inds],"+")
  } else {
    const = 0
  }
    
  # extract coefficients
  ucoefs=vector("list",length(ubases))
  for (i in seq_along(ubases)) {
    inds = which(bases==ubases[[i]])
    ucoefs[[i]] = unflatten.term(coefs[inds],"+")
  }
  
  list(bases=ubases, coefs=ucoefs,const=const)
}

apply.term.rules = function(term, rules, min.length=2, contain.symbol=NULL, dont.contain.symbol=NULL, nested =TRUE, repeated=FALSE, max.counter=200) {
  restore.point("apply.term.rules")
  
  if (length(term)< min.length)
    return(term)

  changed = TRUE
  old.term = term

  counter = 0
  while(changed & counter <= max.counter) {
    counter = counter+1
    changed = FALSE
    # apply rules on children first
    if (nested) {
      #restore.point("apply.term.rules.2")
      for (i in seq_along(term)[-1]) {
        term[[i]] = apply.term.rules(term[[i]], rules=rules, min.length=2, contain.symbol=contain.symbol, dont.contain.symbol=dont.contain.symbol)
      }
    }
    #restore.point("apply.term.rules.3")
  # apply all rules sequentially
  
    for (i in seq_along(rules$old)) {
      pattern = rules$old[[i]]
      m = match.pattern(term,pattern,contain.symbol = contain.symbol, dont.contain.symbol=dont.contain.symbol) 
      if (!identical(m,NA)) {
        cat("\nrule: ", deparse1(pattern), " --> ", deparse1(rules$new[[i]]),"")
        cat("\n       ",deparse1(term))
        term = substitute.call(rules$new[[i]], m)
        cat(" changed to", deparse1(term),"\n.")
        #stop()
      }
    }
    
    changed = (!identical(term, old.term)) & (repeated)
    old.term = term
  }
  if (counter >= max.counter)
    warning("reached max.counter")
  term
}


examples.solve_symb = function() {
  sym.solve.eq(quote((1+3+4*x)+z == 5),"x")
  sym.solve.eq(quote((1+3*x+4*x)+z == 5),"x")
  sym.solve.eq(quote((1+3+4*log(x))+z == 5),"x")
}


sym.solve.eq= function(eq, var) {
  restore.point("sym.solve.eq")
  
  term = substitute(lhs - (rhs), list(lhs=eq[[2]],rhs=eq[[3]]))
  poly = term.to.poly(term,var = var,is.expanded = FALSE)
  sol = solve.poly(poly)
  
  if (!sol$solved) return(sol)
  eq = sol$eq
  symbol = as.name(var)
  if (!identical(eq[[2]],symbol)) {    
    sol = isolate.symbol(eq,symbol = symbol)
  }
  sol  
}


isolate.symbol = function(eq,symbol, rules=get.syso()$isolate.rules) {
  restore.point("isolate.symbol")
  
  eq = substitute((lhs - (rhs)) == 0, list(lhs=eq[[2]],rhs=eq[[3]]))  

  old.eq = eq
  
  ok = FALSE
  i = 1
  contain.symbol.li = list(.x=symbol)
  while(TRUE) {
    for (i in seq_along(rules$old)) {
      pattern = rules$old[[i]]
      m = match.pattern(eq,pattern,contain.symbol = contain.symbol.li) 
      if (!identical(m,NA)) {
        eq = substitute.call(rules$new[[i]], m)
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


match.pattern = function(call, pattern, check.identical=TRUE, contain.symbol=NULL, dont.contain.symbol=NULL) {
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
    if (!are.symbols.not.contained(li[[i]],names(li)[i],dont.contain.symbol))
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


are.symbols.not.contained = function(call, placeholder, dont.contain.symbol) {
  restore.point("are.symbols.not.contained")
  cs = dont.contain.symbol[[placeholder]]
  if (length(cs)>0) {
    return(!contains.symbol(call = call,symbol = cs))
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
