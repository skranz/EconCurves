

latex.to.textspan = function(str) {
  #str = "x_{5ab} y_{1} z_3"
  restore.point("latex.to.textspan")

  #str = "x_{5ab}\\alpha * \\beta"

  str = replace.latex.with.unicode(str)  

  li = find.subscripts(str)$s
  if (length(li)==1) {
    txt = li
  } else {
    mat = matrix(li,ncol=2,byrow = TRUE)
    txt = paste0(collapse="\n",
      '<tspan>',mat[,1],
        '<tspan dy="5px" class="label_subscript">', mat[,2],'</tspan>',
      '</tspan>'
    )
    
    # remove curley braces
    txt = gsub("{{","jJj",txt, fixed=TRUE)
    txt = gsub("}}","hHh",txt, fixed=TRUE)
    txt = gsub("{","",txt, fixed=TRUE)
    txt = gsub("}"," ",txt, fixed=TRUE)
    txt = gsub("  "," ",txt, fixed=TRUE)
    txt = gsub("jJj","{{",txt, fixed=TRUE)
    txt = gsub("hHh","}}",txt, fixed=TRUE)
  }

  txt
}

find.subscripts = function(str) {
  restore.point("find.subscripts")
  

  # find subscripts
  pos1 = str.find(str,'_[0-9a-zA-Z_|.=]+',fixed=FALSE)
  pos2 = str.find(str,'_\\{[0-9a-zA-Z_|.=,]+\\}',fixed=FALSE)
  pos = rbind(pos1,pos2)
  if (NROW(pos)==0) {
    return(list(s=str,is.sub=FALSE))
  }
  
  spl = str.split.at.pos(str,pos,keep.pos = TRUE)  
  first = pos[1,1]==1
  if (first) {
    is.sub = rep(c(TRUE,FALSE),length.out=length(spl))
  } else {
    is.sub = rep(c(FALSE,TRUE),length.out=length(spl))
  }
  spl[is.sub] = substring(spl[is.sub],2)

  
    
  list(s=spl, is.sub=is.sub)

}

replace.latex.with.unicode = function(str) {

  latex = c( "\\alpha","\\beta","\\gamma","\\delta","\\epsilon","\\zeta","\\eta","\\theta","\\iota","\\kappa","\\lambda","\\mu","\\nu","\\xi","\\pi","\\rho","\\varsigma","\\sigma","\\tau","\\upsilon","\\phi","\\chi","\\psi","\\omega","\\Gamma","\\Delta","\\Theta","\\Lambda","\\Xi","\\Pi","\\Sigma","\\Upsilon","\\Phi","\\Psi","\\Omega","\\neg","\\pm","\\cdot","\\to","\\Rightarrow","\\Leftrightarrow","\\forall","\\partial","\\exists","\\emptyset","\\nabla","\\in","\\notin","\\prod","\\sum","\\surd","\\infty","\\wedge","\\vee","\\cap","\\cup","\\int","\\approx","\\neq","\\equiv","\\leq","\\geq","\\subset","\\supset","\\^circ","\\times","\\lfloor","\\rfloor","\\lceil","\\rceil" ) 
  
uc = c( "\U3B1","\U3B2","\U3B3","\U3B4","\U3B5","\U3B6","\U3B7","\U3B8","\U3B9","\U3BA","\U3BB","\U3BC","\U3BD","\U3BE","\U3C0","\U3C1","\U3C2","\U3C3","\U3C4","\U3C5","\U3C6","\U3C7","\U3C8","\U3C9","\U393","\U394","\U398","\U39B","\U39E","\U3A0","\U3A3","\U3A5","\U3A6","\U3A8","\U3A9","\U00AC","\U00B1","\U00B7","\U2192","\U21D2","\U21D4","\U2200","\U2202","\U2203","\U2205","\U2207","\U2208","\U2209","\U220F","\U2211","\U221A","\U221E","\U2227","\U2228","\U2229","\U222A","\U222B","\U2248","\U2260","\U2261","\U2264","\U2265","\U2282","\U2283","\U00B0","\U00D7","\U230A","\U230B","\U2308","\U2309" )
  
  pos = str.find(str,'\\\\[0-9a-zA-Z]+',fixed=FALSE)
  spl = str.split.at.pos(str,pos,keep.pos = TRUE)  
  ind = match(spl, latex)
  rows = !is.na(ind)
  spl[rows] = uc[ind[rows]]
  
  res = paste0(spl,collapse="")
  Encoding(res) = "UTF-8"
  res
}

make.greece.code = function() {  
  str='
  α,alpha,&alpha;,x3B1
  β,beta,&beta;,x3B2
  γ,gamma,&gamma;,x3B3
  δ,delta,&delta;,x3B4
  ε,epsilon,&epsilon;,x3B5
  ζ,zeta,&zeta;,x3B6
  η,eta,&eta;,x3B7
  θ,theta,&theta;,x3B8
  ι,iota,&iota;,x3B9
  κ,kappa,&kappa;,x3BA
  λ,lambda,&lambda;,x3BB
  μ,mu,&mu;,x3BC
  ν,nu,&nu;,x3BD
  ξ,xi,&xi;,x3BE
  π,pi,&pi;,x3C0
  ρ,rho,&rho;,x3C1
  ς,varsigma,&sigmaf;,x3C2
  σ,sigma,&sigma;,x3C3
  τ,tau,&tau;,x3C4
  υ,upsilon,&upsilon;,x3C5
  φ,phi,&phi;,x3C6
  χ,chi,&chi;,x3C7
  ψ,psi,&psi;,x3C8
  ω,omega,&omega;,x3C9
  Γ,Gamma,&Gamma;,x393
  Δ,Delta,&Delta;,x394
  Θ,Theta,&Theta;,x398
  Λ,Lambda,&Lambda;,x39B
  Ξ,Xi,&Xi;,x39E
  Π,Pi,&Pi;,x3A0
  Σ,Sigma,&Sigma;,x3A3
  Υ,Upsilon,&Upsilon;,x3A5
  Φ,Phi,&Phi;,x3A6
  Ψ,Psi,&Psi;,x3A8
  Ω,Omega,&Omega;,x3A9
¬,neg,&not;,x00AC
±,pm,&plusmn;,x00B1
·,cdot,&middot;,x00B7
→,to,&rarr;,x2192
⇒,Rightarrow,&rArr;,x21D2
⇔,Leftrightarrow,&hArr;,x21D4
∀,forall,&forall;,x2200
∂,partial,&part;,x2202
∃,exists,&exist;,x2203
∅,emptyset,&empty;,x2205
∇,nabla,&nabla;,x2207
∈,in,&isin;,x2208
∉,notin,&notin;,x2209
∏,prod,&prod;,x220F
∑,sum,&sum;,x2211
√,surd,&radic;,x221A
∞,infty,&infin;,x221E
∧,wedge,&and;,x2227
∨,vee,&or;,x2228
∩,cap,&cap;,x2229
∪,cup,&cup;,x222A
∫,int,&int;,x222B
≈,approx,&asymp;,x2248
≠,neq,&ne;,x2260
≡,equiv,&equiv;,x2261
≤,leq,&le;,x2264
≥,geq,&ge;,x2265
⊂,subset,&sub;,x2282
⊃,supset,&sup;,x2283
°,^circ,&deg;,x00B0
×,times,&times;,x00D7
⌊,lfloor,&lfloor;,x230A
⌋,rfloor,&rfloor;,x230B
⌈,lceil,&lceil;,x2308
⌉,rceil,&rceil;,x2309'
  d = read.csv(textConnection(str),header = FALSE,stringsAsFactors = FALSE)
  
  cat("latex = c(",paste0('"\\\\',d[,2],'"',collapse=","),")")
  uc = d[,4]
  cat("uc = c(",paste0('"\\U',substring(uc,2),'"',collapse=","),")")
}