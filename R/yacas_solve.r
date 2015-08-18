
examples.yacas.solve.eq = function() {
  yacas.solve.eq(eq=quote((1+3+4*x)+z == 5),"x")
  yacas.solve.eq(eq=quote(x^2 == 1),"x")

  eqs = list(
    quote(x+y==5),
    quote(y+5*x+y==x+z)
  )
  vars = c("x,","y")
  sympy.solve.eqs(eqs, vars=c("x","y"))

  
  yacas.simplify("2*x+x == x")
  class(yacas.simplify("2*x+x"))
  
  term = "x+x+x"
}

maxima.simplify = function(term) {
  restore.point("maxima.simplify")
  library(RMaxima)
  if (is.call(term))
    term = deparse1(term)
  res = mx.simplify(str = term,simplify.code = "ratsimp()")
  res.str = res$li[[1]]
  parse.as.call(res.str)
}

yacas.simplify = function(term) {
  restore.point("yacas.simplify")
  library(Ryacas)
  if (!is.character(term))
    term = deparse1(term)
  code = paste0("Simplify(",term,")")
  Ryacas::yacas(code)[[1]][[1]]
}

yacas.solve.eq = function(eq,var) {
  restore.point("yacas.solve.eq")
  library(Ryacas)

  #eq=quote((1+3+4*x)+z^2 == a+b)
  #var = "b"
  #yacas("Solve(x/(1+x) == a, x)")
  if (is.character(eq)) {
    eq.str = eq
  } else {
    eq.str = deparse1(eq)
  }
  code = paste0("Solve(",eq.str,", ", var,")")
  res = yacas(code)
  #sol.eq = res[[1]][[1]]
  solved = length(res[[1]][[1]]) > 1
  if (solved) {
    sol.eq = res[[1]][[1]][[2]]
  } else {
    sol.eq = NULL
  }
  list(solved=solved, eq = sol.eq)
}
