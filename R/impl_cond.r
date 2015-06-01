examples.impl_cond = function() {
  cond = quote(sum(R)<= Smax)
  
  cond = quote((sum(R) == Smax) | ((sum(R) <= Smax) & all(m==0))) 
  
  
  cond = quote(
     (all(R >= 0)) &
        (
          (sum(R) == first(Smax)) | 
          ((sum(R)<= first(Smax)) & all(m==0))
        )
  )
  adapt.to.implicit.eq(cond)
  
  pmax(
    max(abs(max(0 - (R), 0))),
    pmin(
      abs(sum(R) - (first(Smax))), 
      pmax(
        max(sum(R) - (first(Smax)), 0),
        max(abs(abs(m - (0))))
      )
    )
  )
  
  pryr:::call_tree(cond)
  res = change.nested.call(cond,adapt.call=adapt.to.implicit.eq)
  res
    
  cond = quote(1+2+(1+3*x))
  pryr:::call_tree(cond)
}

# transform equality or inequality, possibly with logical operators
# and all or any to a implicit lhs condition that can be used in
# nleqslv
adapt.to.implicit.eq = function(x,nested=TRUE, squared=TRUE) {
  restore.point("adapt.to.implicit.eq")
  if (is.symbol(x)) return(x)
  name = as.character(x[[1]])
  
  if (squared) {
    if (name=="==") {
      x = substitute((lhs-rhs)^2,list(lhs=x[[2]],rhs=x[[3]]))
    } else if (name=="<=") {
      x = substitute(max(lhs-(rhs),0)^2,list(lhs=x[[2]],rhs=x[[3]]))
    } else if (name==">=") {
      x = substitute(max(rhs-(lhs),0)^2,list(lhs=x[[2]],rhs=x[[3]]))
    } else if (name == "all" | name == "any") { 
      inner = x[[2]]
      if (nested)
        inner = adapt.to.implicit.eq(inner, nested=TRUE)
      
      if (name=="all") {
        x = substitute(max((inner)^2),list(inner=inner))
      } else if (name=="any") {
        x = substitute(min((inner)^2),list(inner=inner))
      }
    } else if (name=="|" | name=="||" | name=="&" | name=="&&") { 
      lhs = x[[2]]
      rhs = x[[3]]
      
      if (nested) {
        lhs = adapt.to.implicit.eq(lhs, nested=TRUE)
        rhs = adapt.to.implicit.eq(rhs, nested=TRUE)
      }
      if (name=="|" | name=="||") {
        x = substitute(pmin(lhs,rhs),list(lhs=lhs,rhs=rhs))
      } else if (name=="&" | name=="&&") {
        x = substitute(pmax(lhs,rhs),list(lhs=lhs,rhs=rhs))
      }
    } else if (name=="(" | nested) {
      x = adapt.to.implicit.eq(x[[2]], nested=nested)
      #x=substitute((inner), list(inner=inner))
    }
    
  } else {
    if (name=="==") {
      x = substitute(abs(lhs-(rhs)),list(lhs=x[[2]],rhs=x[[3]]))
    } else if (name=="<=") {
      x = substitute(max(lhs-(rhs),0),list(lhs=x[[2]],rhs=x[[3]]))
    } else if (name==">=") {
      x = substitute(max(rhs-(lhs),0),list(lhs=x[[2]],rhs=x[[3]]))
    } else if (name == "all" | name == "any") { 
      inner = x[[2]]
      if (nested)
        inner = adapt.to.implicit.eq(inner, nested=TRUE)
      
      if (name=="all") {
        x = substitute(max(abs(inner)),list(inner=inner))
      } else if (name=="any") {
        x = substitute(min(abs(inner)),list(inner=inner))
      }
    } else if (name=="|" | name=="||" | name=="&" | name=="&&") { 
      lhs = x[[2]]
      rhs = x[[3]]
      
      if (nested) {
        lhs = adapt.to.implicit.eq(lhs, nested=TRUE)
        rhs = adapt.to.implicit.eq(rhs, nested=TRUE)
      }
      if (name=="|" | name=="||") {
        x = substitute(pmin(lhs,rhs),list(lhs=lhs,rhs=rhs))
      } else if (name=="&" | name=="&&") {
        x = substitute(pmax(lhs,rhs),list(lhs=lhs,rhs=rhs))
      }
    } else if (name=="(" | nested) {
      x = adapt.to.implicit.eq(x[[2]], nested=nested)
      #x=substitute((inner), list(inner=inner))
    }
    
  }
  x
}


#' recursively change element
change.nested.call <- function(x, adapt.atomic=NULL, adapt.name=NULL, adapt.call=NULL, pre.adapt.call=NULL, adapt.pairlist=NULL,...) {
  if (is.atomic(x)) {
    if (!is.null(adapt.atomic))
      x = adapt.atomic(x,...)
    return(x)
  } else if (is.name(x)) {
    if (!is.null(adapt.name))
      x = adapt.name(x,...)
    return(x)
  } else if (is.call(x)) {
    if (!is.null(pre.adapt.call))
      x = pre.adapt.call(x,...)
    for (i in seq_along(x)) {
      x[[i]] = change.nested.call(x[[i]],adapt.atomic, adapt.name, adapt.call, adapt.pairlist,...)
    }
    if (!is.null(adapt.call))
      x = adapt.call(x,...)

    return(x)
  } else if (is.pairlist(x)) {
    if (!is.null(adapt.pairlist))
      x = adapt.pairlist(x,...)
    for (i in seq_along(x)) {
      x[[i]] = change.nested.call(x[[i]],adapt.atomic, adapt.name, adapt.call, adapt.pairlist,...)
    }
    return(x)
  } else {
    stop("Don't know how to handle type ", typeof(x), 
      call. = FALSE)
  }
}