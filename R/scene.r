examples.new.scene = function() {
  IS = curve("y  = A - a*r",
             xvar="y",yvar="r", color="red")
  
  PC = curve("pi = pi_expect +alpha*(y-y_eq)",
             xvar="y",yvar="pi", color="blue")
    
  MR = curve("y  = y_e -alpha*beta*(pi-pi_target)",
             xvar="y",yvar="pi", color="green")
  
  init.par = list(
    # Exogenous parameters
    A=1,              # Intercept IS
    a = 1,            # abs slope IS
    alpha = 1,        # slope PC
    y_eq  = 0.5,      # long run equilibrium output 
    beta = 1,         # central bank weight on output gap
    pi_target = 2     # target inflation rate    
  )
  
  eq.var = list(
    # Initial variable values in equilibrium
    r_t  = "y-value of IS at y_eq",
    y_t  = "y_eq",
    pi_t = "pi_target",
    pi_expect = "pi_target"    
  )
  

  IS.sp = new.scene.pane(xvar="y", yvar="r", curves="IS", grid.pos = c(1,1)) 
  PC.MR.sp = new.scene.pane(xvar="y", yvar="pi", curves=c("PC","MR"), grid.pos = c(1,2))
  
  ranges = list(
    y  = c(-5,5),
    r  = c(-1,5),
    pi = c(-1,10)
  )
  
  scene = new.scene(
    curves = nlist(IS, PC, MR),
    scene.panes = nlist(IS.sp, PC.MR.sp),
    par = par,
    ranges = ranges 
  )
}

new.scene.pane = function(...) {
  list(...)
}

new.scene = function(curves, scene.panes, ranges=NULL, init.par=NULL, equ.var=NULL, ...) {
  sc = nlist(curves=curves, ranges=ranges, par=par, scene.panes=scene.panes, ...)  
}