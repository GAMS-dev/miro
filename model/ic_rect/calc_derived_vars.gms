$ontext
Calculate line power and objective value from P,Q,V_real,V_imag
$offtext

V_LineP.l(i,j,c)$branchstatus(i,j,c) =
    (g(i,j,c) / sqr(ratio(i,j,c)))
        * (sqr(V_real.l(i)) + sqr(V_imag.l(i)))
    - (1 / ratio(i,j,c))
        * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(i)*V_real.l(j) + V_imag.l(i)*V_imag.l(j))
           +(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(j)*V_imag.l(i) - V_real.l(i)*V_imag.l(j)))
;

V_LineP.l(j,i,c)$branchstatus(i,j,c) =
    g(i,j,c)
        * (sqr(V_real.l(j)) + sqr(V_imag.l(j)))
    - (1 / ratio(i,j,c))
        * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(j)*V_real.l(i) + V_imag.l(j)*V_imag.l(i))
           +(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(i)*V_imag.l(j) - V_real.l(j)*V_imag.l(i)))
;

V_LineQ.l(i,j,c)$branchstatus(i,j,c) =
    - ((b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
        * (sqr(V_real.l(i)) + sqr(V_imag.l(i)))
    - (1 / ratio(i,j,c))
        * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(j)*V_imag.l(i) - V_real.l(i)*V_imag.l(j))                 
           -(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(i)*V_real.l(j) + V_imag.l(i)*V_imag.l(j)))
;

V_LineQ.l(j,i,c)$branchstatus(i,j,c) =
    - (b(i,j,c) + bc(i,j,c)/2)
        * (sqr(V_real.l(j)) + sqr(V_imag.l(j)))
    - (1 / ratio(i,j,c))
        * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(i)*V_imag.l(j) - V_real.l(j)*V_imag.l(i))                 
           -(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                * (V_real.l(j)*V_real.l(i) + V_imag.l(j)*V_imag.l(i)))
;

V_objcost.l = sum(gen,
      costcoef(gen,'3')
      + costcoef(gen,'2')*V_P.l(gen)*baseMVA
      + costcoef(gen,'1')*sqr(V_P.l(gen)*baseMVA))
;

