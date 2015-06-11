


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
-(.a+.b)     --> -.a + -(.b)
(.a) + .b    --> .a + .b
.a + (.b)    --> .a + .b

'
    syso.env$expand.rules = parse.term.rules(rules.txt)

      rules.txt = 
'
.x * .x      --> .x^2

.x * .x^.b   --> .x^(.b+1)
.x^.a * .x^.b   --> .x^(.a+.b)
.x^.a * .x   --> .x^(.a+1)

.a / .x      --> .x^-1 * .a
.x / .a      --> .x * (1/.a)
'
    syso.env$normalize.prod = parse.term.rules(rules.txt)

          rules.txt = 
'
.a - .b  --> .a + -.b
.a / .b  --> .a * .b^-1
'
    syso.env$norm.minus.div = parse.term.rules(rules.txt)
 
    rules.txt = 
'
1^.a  --> 1
.a*1  --> .a
1*.a  --> .a
0+.a  --> .a
.a+0  --> .a
'
    syso.env$simplify = parse.term.rules(rules.txt)
    
    
  }
}

get.syso = function() {
  init.syso()
  syso.env
}



examples.solve_symb = function() {
  sym.solve.eq(quote((1+3+4*x)+z == 5),"x")
  sym.solve.eq(quote((1+3*x+4*x)+z == 5),"x")
  sym.solve.eq(quote((1+3+4*log(x))+z == 5),"x")
  
  sym.solve.eq(quote(y_mr - (A - a * r_)),"r_")
  
  eqs = list(
    quote(x+y==5),
    quote(y+5*x+y==x+z)
  )
  sym.solve.eqs(eqs, vars=c("x","y"))
}


sym.solve.eqs = function(eqs, vars) {
  restore.point("sym.solve.eqs")
  
  n = length(vars)
  
  symbols = lapply(vars, as.symbol)
  subst.lists = vector("list", n)
  names(subst.lists) = vars
  for (i in seq_along(eqs)) {
    eq = eqs[[i]]
    if (i >1) {
      for (j in 1:(i-1)) {
        eq = substitute.call(eq, subst.lists[j])    
      }
    }
    sol = sym.solve.eq(eq, vars[[i]])
    if (!sol$solved)
      return(list(solved=FALSE, eqs=eqs))
    eqs[[i]] = sol$eq
    subst.lists[[i]] = sol$eq[[3]]
  }
  
  if (n>1) {
    for (i in (n-1):1) {
      eq = eqs[[i]]
      for (j in (i+1):n) {
        eq = substitute.call(eq, subst.lists[j])    
      }
      sol = sym.solve.eq(eq, vars[[i]])
      if (!sol$solved)
        return(list(solved=FALSE, eqs=eqs))
      eqs[[i]] = sol$eq
    }  
  }
  
  names(eqs) = vars
  list(solved=TRUE, eqs=eqs)
}


sym.solve.eq= function(eq, var) {
  restore.point("sym.solve.eq")
  
  term = substitute(lhs - (rhs), list(lhs=eq[[2]],rhs=eq[[3]]))
  term = apply.term.rules(term = term,rules = get.syso()$normalize.prod, repeated=TRUE)
  
  poly = term.to.poly(term,var = var,is.expanded = FALSE)
  sol = solve.poly(poly)
  
  if (!sol$solved) return(sol)
  eq = sol$eq
  
  #eq = apply.term.rules(eq,rules = get.syso()$simplify, repeated=TRUE,nested = TRUE)
  symbol = as.name(var)
  if (!identical(eq[[2]],symbol)) {    
    sol = isolate.symbol(eq,symbol = symbol)
  }
  sol  
}


examples.expand.term = function() {
  term = quote(y + 5 * (1^-1 * (y + -(5))) + y - (1^-1 * (y + -(5)) + z))
  expand.term(term)

  term = quote(y + 5 * (1^-1 * (y + -(5))) + y - (3 * (y + -(5)) + z))
  term = quote(y - (3 * (y + -(5)) + z))
  expand.term(term)

    
  term = quote(3+(3+x)*(y-1)*(a+1) + 3*(1+1)-5 / 2)
  term = quote(-(3+x)*4 - (3 * (y + -(5))))
  expand.term(term)
  term = quote( - (2 * (y + 5) + z))
  expand.term(term)

}

repeat.fun.until.no.change = function(fun,x, max.counter=Inf,...) {
  restore.point("repeat.until.no.change")
  old.x = x
  counter = 0
  while(counter<max.counter) {
    counter = counter+1
    x  = fun(x,...)
    if (identical(x,old.x))
      return(x)
    old.x = x
  }
}



expand.unary.minus = function(term, repeated=FALSE) {
  #restore.point("expand.unary.minus")
  if (repeated)
    return(repeat.fun.until.no.change(expand.unary.minus,term))
  

  if (is.name(term)) return(term)
  
  if (term[[1]] != "-" | length(term)!=2) return(term)
  if (is.name(term[[2]])) return(term)
  act = term[[2]]
  while(TRUE) {
    if (is.name(act)) break
    if (act[[1]] != "(") break
    act = act[[2]]
  }
  if (is.name(act))
    return(substitute(-a,list(a=act)))
  
  if (act[[1]]=="+") {
    li = flatten.term(act,"+")
    li = lapply(li, function(sterm) {
      substitute(-a, list(a=sterm))
    })
    return(unflatten.term(li,"+"))
  } else if (act[[1]]=="*") {
    act[[2]] = substitute(-a, list(a=act[[2]]))
    return(act)
  # two unary minus negate each other
  } else if (act[[1]]=="-" & length(act)==2){
    return(substitute(a,list(a=act[[2]])))
  } else {
    return(substitute(-a,list(a=act)))
  }
}

flatten.term.and.unary.minus = function(term, op="+") {
  #restore.point("flatten.term.and.unary.minus")
  term = expand.unary.minus(term)
  li = flatten.term(term,op)
  old = li
  while (TRUE) {
     li = lapply(li, expand.unary.minus)
     if (identical(li,old)) break
     
     res.li = lapply(li,flatten.term,op)
     li = do.call(c,res.li)
     old = li
  }
  li
}

expand.term = function(term,  rules = get.syso()$expand.rules, return.flat=FALSE) {
  #restore.point("expand.term")
  
  
  # change .a - .b to .a + -.b
  rules = get.syso()$norm.minus.div
  term = apply.term.rules(term, rules, repeated=TRUE)

  li = flatten.term.and.unary.minus(term,"+")

  old = li
  sterm = li[[2]]
  while(TRUE) {  
    li = lapply(li, function(sterm) {
      factors = flatten.term.and.unary.minus(sterm,op="*")
      #factors = lapply(factors, expand.unary.minus)
      if (length(factors)==1) return(list(sterm))
      
      summands = lapply(factors,flatten.term.and.unary.minus, op="+")
      len = sapply(summands, length)
      if (prod(len)==1) return(list(sterm))
      
      # expand factors
      left = summands[[1]]
      s = 2
      for (s in 2:length(summands)) {
        right = summands[[s]]
        left = lapply(1:(length(left)*length(right)), function(i) {
          ai = arrayInd(i, .dim=c(length(left),length(right)))
          substitute(le*ri, list(le=left[[ai[1]]],ri=right[[ai[2]]]))
        })
      }
      left
    })
    li = do.call(c,li)
    if (identical(old, li)) break
    old = li
  }
  if (return.flat)
    return(li)
  
  unflatten.term(li,op="+")
}


example.flatten.term = function() {
  term = quote(3+5+x+(2+4)+3*(4+6))
  flatten.to(term)
  flatten.term(term)
}

flatten.to = function(call, from="+", to="fplus", nested=TRUE) {
  if (length(call)<=1) return(call)
  if (term[[1]]==from) {
    li = flatten.term(call, op=from)
    return(as.call(c(list(as.symbol(to)),li)))
  }
  return(call)
}

flatten.term = function(term, op="+", flatten.brackets=TRUE) {
  if (length(term)<=1) return(list(term))
  if (term[[1]]==op | (term[[1]]=="(" & flatten.brackets)) {
    li = lapply(term[-1], flatten.term, op=op)
    return(do.call(c,li))
  }
  return(list(term))
}

unflatten.term = function(term, op="+") {
  #restore.point("unflatten.term")
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
  
  minus.const = substitute(-const,list(const=poly$const))
  if (identical(poly$const, 0)) {
    rhs = 0
  } else if (identical(poly$coefs[[1]],1)) {
    rhs = minus.const
  } else {
    rhs = substitute(minus.const/coef, list(minus.const=minus.const,coef=poly$coefs[[1]]))
  }
  eq = substitute(lhs==rhs,list(lhs=lhs,rhs=rhs))
  list(solved=TRUE, eq=eq)
}

term.to.poly = function(term, var, is.expanded=FALSE) {
  #restore.point("term.to.poly")
  symbol= as.symbol(var)  

  # expand term and return flatten sum
  terms = expand.term(term,return.flat = TRUE)

  # flatten sums and then products
  #terms = flatten.term(term,"+")
  terms = lapply(terms, flatten.term, op="*")

  # compute bases and coefs 
  factors.base.coef = function(factors, symbol) {
    has.symbol = sapply(factors, contains.symbol, symbol=symbol)      
    base = unflatten.term(factors[has.symbol],"*")
    coef = unflatten.term(factors[!has.symbol],"*")
    res = normalize.base(base=base, coef=coef)
    res
  }
  bc.li = lapply(terms, factors.base.coef, symbol=symbol)
  bases = lapply(bc.li, function(bc) bc$base)  
  coefs = lapply(bc.li, function(bc) bc$coef)  

  
  #bc.df = data_frame(bases, coefs)
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
    inds = which(sapply(bases, function(base) identical(base,ubases[[i]])))
    coef = unflatten.term(coefs[inds],"+")
    ucoefs[[i]] = coef
  }

  
  list(bases=ubases, coefs=ucoefs,const=const)
}

normalize.base = function(base, coef) {
  #restore.point("normalize.base")
  if (is.null(base)) return(list(base=base, coef=coef))
  if (is.null(coef)) coef = 1
  if (is.symbol(base)) return(list(base=base, coef=coef))
  
  
  if (base[[1]]=="-")
    return(list(base=base[[2]], coef=substitute(-coef,list(coef=coef))))
  list(base=base, coef=coef)
}

apply.term.rules = function(term, rules, min.length=2, contain.symbol=NULL, dont.contain.symbol=NULL, nested =TRUE, repeated=FALSE, max.counter=200) {
  #restore.point("apply.term.rules")
  
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
        #cat("\nrule: ", deparse1(pattern), " --> ", deparse1(rules$new[[i]]),"")
        #cat("\n       ",deparse1(term))
        term = substitute.call(rules$new[[i]], m)
        #cat(" changed to", deparse1(term),"\n.")
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



isolate.symbol = function(eq,symbol, rules=get.syso()$isolate.rules) {
  #restore.point("isolate.symbol")
  
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
  #restore.point("parse.ruls")
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
  
  #restore.point("contains.symbol")
 
  for (i in seq_along(call)) {
    if (contains.symbol(call[[i]], symbol)) return(TRUE)
  }
  
  return(FALSE)
}


match.pattern = function(call, pattern, check.identical=TRUE, contain.symbol=NULL, dont.contain.symbol=NULL) {
  #restore.point("match.pattern")
  
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
  
  #restore.point("match.pattern.2")

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
  #restore.point("are.symbols.contained")
  cs = contain.symbol[[placeholder]]
  if (length(cs)>0) {
    return(contains.symbol(call = call,symbol = cs))
  }
  return(TRUE)
}


are.symbols.not.contained = function(call, placeholder, dont.contain.symbol) {
  #restore.point("are.symbols.not.contained")
  cs = dont.contain.symbol[[placeholder]]
  if (length(cs)>0) {
    return(!contains.symbol(call = call,symbol = cs))
  }
  return(TRUE)
}


are.same.placeholders.identical = function(li) {
  #restore.point("are.same.vars.identical")
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
