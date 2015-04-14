# 3 curves can be specified in 3 possible forms

# y.formula:      y = f(x)
# x.formula:      x = g(y)
# level.formula:  level = h(x,y)

# For simplicity formulas will be stored as strings


old.examples.curve = function() {
  IS = curve("y  = A - a*r",
             xvar="y",yvar="r", color="red")
  
  PC = curve("pi = pi_E +alpha*(y-y_e)",
             xvar="y",yvar="pi", color="blue")
    
  MR = curve("y  = y_e -alpha*beta*(pi-pi_T)",
             xvar="y",yvar="pi", color="green")

  MRi = curve("y-y_e  = -alpha*beta*(pi-pi_T)",
           xvar="y",yvar="pi", color="green")

  
  par = list(A=1,        # Intercept IS
          a = 1,      # abs slope IS
          pi_E = 2,   # expected Inflation
          alpha = 1,  # slope PC
          y_e  = 0.5,   # long run equilibrium output 
          beta = 1,   # central bank weight on output gap
          pi_T = 2    # target inflation rate
        )
  
  theme = list(lwd=2, axis.lwd=2)
  
  yrange  = c(-5,5)
  rrange  = c(-1,5)
  pirange = c(-1,10)
  
  vpp1 = list(x1=0.1, y1=0.55, x2 = 0.9, y2 = 0.95)
  vpp2 = list(x1=0.1, y1=0.1, x2 = 0.9, y2 = 0.5)

  pane1 = new.pane(xvar="y",yvar="r", curves=nlist(IS),
                   xrange=yrange, yrange=rrange, vp.pos=vpp1
                   )
  
  pane2 = new.pane(xvar="y",yvar="pi", curves=nlist(PC,MR),
                   xrange=yrange, yrange=pirange, vp.pos=vpp2)

  grid.newpage()
  popViewport(0)
  pane1 = draw.pane(pane1, par=par, extend.ranges=FALSE)
  pane2 = draw.pane(pane2, par=par, extend.ranges=FALSE)
  
  plot(pane1$curves.xy[[1]]$x,pane1$curves.xy[[1]]$y)
  
  compute.points(IS,xrange=yrange, yrange=rrange, par=par)
  compute.points(MRi,xrange=yrange, yrange=rrange, par=par)
  compute.points(MR,xrange=yrange, yrange=rrange, par=par)

  
  library(codetools)
  ca = parse(text = "pi = pi_E +alpha*(y-y_e)+max(4,5)")[[1]]
  find.variables(ca)
}

parse.formula = function(formula) {
  parse(text=formula)[[1]]
}

curve = function(formula, xvar, yvar, level=NULL, pal=NULL, par = NULL, color=NULL) {
  restore.point("curve")
  f = specialize.curve.formula(formula, xvar,yvar, level)
  cu = c(f,nlist(xvar,yvar,level=level, pal=pal, par=NULL, color=color))
  class(cu) = c("Curve","list")
  cu
}

specialize.curve.formula = function(formula, xvar, yvar, level=NULL) {
  restore.point("specizalize.curve.formula")
  formula_ = parse.formula(formula)
  lhs_ = get.lhs(formula_)
  rhs_ = get.rhs(formula_)
  
  vl = find.variables(lhs_)
  vr = find.variables(rhs_)

  yformula = yformula_ = xformula = xformula_ = NULL
  
  # y variable is alone on lhs
  if (identical(vl,yvar) & (! yvar %in% vr)) {
    yformula_ = formula_
    yformula  = formula
  }

  # x variable is alone on lhs
  if (identical(vl,xvar) & (! xvar %in% vr)) {
    xformula_ = formula_
    xformula  = formula
  }
  
  # implicit formula
  lhs = str.left.of(formula,"=")
  rhs = str.right.of(formula,"=")
  implicit = paste0(lhs, "-(", rhs,")")
  implicit_ = parse.formula(implicit)
  
  ret = nlist(formula, xformula, yformula, implicit,
              formula_, xformula_, yformula_, implicit_)
  ret
}
