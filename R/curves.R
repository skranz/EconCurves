# 3 curves can be specified in 3 possible forms

# y.formula:      y = f(x)
# x.formula:      x = g(y)
# level.formula:  level = h(x,y)

# For simplicity formulas will be stored as strings

parse.formula = function(formula) {
  parse(text=formula)[[1]]
}

curve = function(formula, xvar, yvar, level=NULL, pal=NULL, par = NULL, color=NULL) {
  restore.point("curve")
  formula_ = parse.as.call(formula)
  f = specialize.curve.formula(formula_, xvar,yvar, level)
  cu = c(f,nlist(xvar,yvar,level=level, pal=pal, par=NULL, color=color))
  class(cu) = c("Curve","list")
  cu
}
