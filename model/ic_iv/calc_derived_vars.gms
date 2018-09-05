$ontext
Calculate line power and objective value from P,Q,V_real.l,V_imag.l
$offtext


V_LineIr.l(i,j,c)$branchstatus(i,j,c) =
            1/sqr(ratio(i,j,c))
                * (g(i,j,c)*V_real.l(i) - (b(i,j,c) + bc(i,j,c)/2)*V_imag.l(i))
            - 1/ratio(i,j,c)
                * (  (g(i,j,c)*V_real.l(j) - b(i,j,c)*V_imag.l(j))*cos(angle(i,j,c))
                   + (g(i,j,c)*V_imag.l(j) + b(i,j,c)*V_real.l(j))*sin(angle(i,j,c))
                  )
;

V_LineIr.l(j,i,c)$branchstatus(i,j,c) =
            (g(i,j,c)*V_real.l(j) - (b(i,j,c) + bc(i,j,c)/2)*V_imag.l(j))
            - 1/ratio(i,j,c)
                * (  (g(i,j,c)*V_real.l(i) - b(i,j,c)*V_imag.l(i))*cos(angle(i,j,c))
                   - (g(i,j,c)*V_imag.l(i) + b(i,j,c)*V_real.l(i))*sin(angle(i,j,c))
                  )
;

V_LineIq.l(i,j,c)$branchstatus(i,j,c) =
            1/sqr(ratio(i,j,c))
                * (g(i,j,c)*V_imag.l(i) + (b(i,j,c) + bc(i,j,c)/2)*V_real.l(i))
            - 1/ratio(i,j,c)
                * (  (g(i,j,c)*V_imag.l(j) + b(i,j,c)*V_real.l(j))*cos(angle(i,j,c))
                   - (g(i,j,c)*V_real.l(j) - b(i,j,c)*V_imag.l(j))*sin(angle(i,j,c))
                  )
;

V_LineIq.l(j,i,c)$branchstatus(i,j,c) =
            (g(i,j,c)*V_imag.l(j) + (b(i,j,c) + bc(i,j,c)/2)*V_real.l(j))
            - 1/ratio(i,j,c)
                * (  (g(i,j,c)*V_imag.l(i) + b(i,j,c)*V_real.l(i))*cos(angle(i,j,c))
                   + (g(i,j,c)*V_imag.l(i) + b(i,j,c)*V_real.l(i))*sin(angle(i,j,c))
                  )
;


V_objcost.l = sum(gen,
      costcoef(gen,'3')
      + costcoef(gen,'2')*V_P.l(gen)*baseMVA
      + costcoef(gen,'1')*sqr(V_P.l(gen)*baseMVA))
;

