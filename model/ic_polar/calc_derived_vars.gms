$ontext
Calculate line power and objective value from P,Q,V,Theta
$offtext

V_LineP.l(i,j,c)$branchstatus(i,j,c) =
    g(i,j,c) * sqr(V_V.l(i)) / sqr(ratio(i,j,c))
    - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) *
        (  g(i,j,c) * cos(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c))
         + b(i,j,c) * sin(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c)))
;

V_LineP.l(j,i,c)$branchstatus(i,j,c) =
    g(i,j,c) * sqr(V_V.l(j))
    - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) *
        (  g(i,j,c) * cos(V_Theta.l(j) - V_Theta.l(i) + angle(i,j,c))
         + b(i,j,c) * sin(V_Theta.l(j) - V_Theta.l(i) + angle(i,j,c)))
;

V_LineQ.l(i,j,c)$branchstatus(i,j,c) =
    - (sqr(V_V.l(i)) * (b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
    - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) *
        (  g(i,j,c) * sin(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c))
         - b(i,j,c) * cos(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c)))
;

V_LineQ.l(j,i,c)$branchstatus(i,j,c) =
    - (sqr(V_V.l(j)) * (b(i,j,c) + bc(i,j,c)/2))
    - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) *
        (  g(i,j,c) * sin(V_Theta.l(j) - V_Theta.l(i) + angle(i,j,c))
         - b(i,j,c) * cos(V_Theta.l(j) - V_Theta.l(i) + angle(i,j,c)))
;

V_objcost.l = sum(gen,
      costcoef(gen,'3')
      + costcoef(gen,'2')*V_P.l(gen)*baseMVA
      + costcoef(gen,'1')*sqr(V_P.l(gen)*baseMVA))
;

