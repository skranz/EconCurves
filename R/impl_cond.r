examples.impl_cond = function() {
  cond = quote((sum(R) == Smax) | ((sum(R) <= Smax) & all(m==0))) 
  pryr:::call_tree(cond)
  res = change.nested.call(cond,adapt.call=adapt.to.implicit.eq)
  res
    
  cond = quote(1+2+(1+3*x))
  pryr:::call_tree(cond)
}

# transform equality or inequality, possibly with logical operators
# and all or any to a implicit lhs condition that can be used in
# nleqslv
adapt.to.implicit.eq = function(x,name=x[[1]]) {
  restore.point("adapt.call")
  if (name=="==") {
    x = substitute(lhs-(rhs),list(lhs=x[[2]],rhs=x[[3]]))
  } else if (name=="<=") {
    x = substitute(max(lhs-(rhs),0),list(lhs=x[[2]],rhs=x[[3]]))
  } else if (name==">=") {
    x = substitute(max(rhs-(lhs),0),list(lhs=x[[2]],rhs=x[[3]]))
  } else if (name=="all") {
    x = substitute(max(abs(inner)),list(inner=x[[2]]))
  } else if (name=="any") {
    x = substitute(min(abs(inner)),list(inner=x[[2]]))
  } else if (name=="|" | name=="||") {
    x = substitute(min(lhs,rhs),list(lhs=x[[2]],rhs=x[[3]]))
  } else if (name=="&" | name=="&&") {
    x = substitute(max(lhs,rhs),list(lhs=x[[2]],rhs=x[[3]]))
  } 
  x
}


#' recursively change element
change.nested.call <- function(x, adapt.atomic=NULL, adapt.name=NULL, adapt.call=NULL, pre.adapt.call=NULL, adapt.pairlist=NULL) {
  if (is.atomic(x)) {
    if (!is.null(adapt.atomic))
      x = adapt.atomic(x)
    return(x)
  } else if (is.name(x)) {
    if (!is.null(adapt.name))
      x = adapt.name(x)
    return(x)
  } else if (is.call(x)) {
    if (!is.null(pre.adapt.call))
      x = pre.adapt.call(x)
    for (i in seq_along(x)) {
      x[[i]] = recursive.change.call(x[[i]],adapt.atomic, adapt.name, adapt.call, adapt.pairlist)
    }
    if (!is.null(adapt.call))
      x = adapt.call(x)

    return(x)
  } else if (is.pairlist(x)) {
    if (!is.null(adapt.pairlist))
      x = adapt.pairlist(x)
    for (i in seq_along(x)) {
      x[[i]] = recursive.change.call(x[[i]],adapt.atomic, adapt.name, adapt.call, adapt.pairlist)
    }
    return(x)
  } else {
    stop("Don't know how to handle type ", typeof(x), 
      call. = FALSE)
  }
}