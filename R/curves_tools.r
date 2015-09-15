

is.vertical = function(cu) {
  cu$is.vertical
}

is.horizontal = function(cu) {
  cu$is.horizontal
}


specialize.curve.formula = function(eq, xvar, yvar, level=NULL, solve.symbolic = TRUE) {
  restore.point("specizalize.curve.formula")
  formula_ = eq
  lhs_ = get.lhs(formula_)
  lhs = deparse1(lhs_)
  rhs_ = get.rhs(formula_)
  
  vl = find.variables(lhs_)
  vr = find.variables(rhs_)

  yformula_ = xformula_ = NULL
  
  # y variable is alone on lhs
  if (identical(lhs,yvar) & (! yvar %in% vr)) {
    yformula_ = substitute(rhs, list(rhs=rhs_))
  } else if (solve.symbolic) {
    res = sym.solve.eq(eq,yvar, simplify=TRUE)
    if (res$solved)
      yformula_ = res$eq[[3]]
  }

  # x variable is alone on lhs
  if (identical(lhs,xvar) & (! xvar %in% vr)) {
    xformula_ = substitute(rhs, list(rhs=rhs_))
  } else if (solve.symbolic) {
    res = sym.solve.eq(eq,xvar, simplify=TRUE)
    if (res$solved)
      xformula_ = res$eq[[3]]
  }
  
  # implicit formula
  implicit_ = substitute(lhs-(rhs), list(lhs=lhs_,rhs=rhs_))

  ret = nlist(xformula_, yformula_, implicit_)
  ret
}

