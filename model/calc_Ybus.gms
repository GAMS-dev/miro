$title Caclulate Ybus

b(i,j,c)$line(i,j,c) = -x(i,j,c)/(sqr(r(i,j,c))+sqr(x(i,j,c)));
g(i,j,c)$line(i,j,c) =  r(i,j,c)/(sqr(r(i,j,c))+sqr(x(i,j,c)));

yb(i,j,'real')$(not sameas(i,j)) = sum(c$branchstatus(i,j,c), -1/ratio(i,j,c) * (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c))))
                                 + sum(c$branchstatus(j,i,c), -1/ratio(j,i,c) * (g(j,i,c)*cos(-angle(j,i,c)) - b(j,i,c)*sin(-angle(j,i,c))))
;

yb(i,j,'imag')$(not sameas(i,j)) = sum(c$branchstatus(i,j,c), -1/ratio(i,j,c) * (b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c))))
                                 + sum(c$branchstatus(j,i,c), -1/ratio(j,i,c) * (b(j,i,c)*cos(-angle(j,i,c)) + g(j,i,c)*sin(-angle(j,i,c))))
;
yb(i,i,'real') = sum((j,c)$branchstatus(i,j,c), g(i,j,c)/sqr(ratio(i,j,c))) + sum((j,c)$branchstatus(j,i,c), g(j,i,c));
yb(i,i,'imag') = sum((j,c)$branchstatus(i,j,c), 1/sqr(ratio(i,j,c)) * (b(i,j,c)+bc(i,j,c)/2)) + sum((j,c)$branchstatus(j,i,c), b(j,i,c)+bc(j,i,c)/2);
*display yb;
*display Gs,Bs;

