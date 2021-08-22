$TITLE Elasto-hydrodynamic lubrication

$ontext

   Reference: Michael M. Kostreva,
   "Elasto-hydrodynamic lubrication: a non-linear
   complementarity problem", Int. Journal for Num. Methods
   in Fluids (4), 377-397 (1984).

   The lubricant film gap and pressure between two lubricated
   elastic cylinders in line contact is calculated.  When the pressure
   is positive, Reynolds' equation applies and must be satisfied;
   when the pressure is 0, the surfaces may diverge.

   The load (in pounds) is represented by alpha.
   The speed (in rpm) of the cylinders is represented by lambda.
   (alpha, lambda) pairs from the paper are given here:

   alpha:
    100lbs  2.832
    175lbs  3.746
    250lbs  4.477

   table lambda(load, speed)
           500rpm  2500rpm 5000rpm
   100lbs  6.057   30.29   60.57
   175lbs  1.978   9.889   19.78
   250lbs  .9692   4.846   9.692   ;

$offtext


$if not set NPOINTS $set NPOINTS 100
$onExternalInput
Scalar npoints / %NPOINTS% /;
$offExternalInput
$eval NPOINTS npoints

sets
  phdr 'for pressure output'   / 'x','pressure', 'thickness' /
  J0      / 0, 1 * %NPOINTS% /
  J(J0)   / 1 * %NPOINTS% /
  ;
alias (I,J);

$onExternalOutput
table pressureThickness(J0,phdr);
scalars
  alpha  'pressure viscosity coefficient'
  lambda "model parameter used in Reynolds equation"
  ;
$offExternalOutput

$onExternalInput
scalars
  load  'load on cylinder in lbs' / 250 /
  speed 'cylinder RPM'            / 2500 /
  ;
$offExternalInput

scalars
  pi
  N
  delx
  w0      / 0.5 /
  xa      / -3 /
  xf      / 2 /
  pscale  / 0.75 /
  load0   'baseline load'  / 100 /
  speed0  'baseline speed' / 500 /
  alpha0  'baseline alpha' / 2.832 /
  lambda0 'baseline lambda' / 6.057 /
  ;

N = card(J);
delx = (xf - xa) / N;
pi = 4 * arctan(1);

parameters
  klevel
  plevel(J)
  gap(I)
  w(I)
  ;
w(I) = 1$(ord(I) lt N) + 0.5$(ord(I) eq N);

variable  k        'represents contant term in pressure equation';

positive variables  p(J)        'pressure';
* p(0) is assumed to be 0

equations
  psum   'pressures are scaled so that the integral of the pressure over X = pi/2'
  reynolds(I);

psum .. 1 =e= 2/pi * sum(J, w(J) * p(J)) * delx;

reynolds(I) ..
lambda/delx * (
( power(xa + (ord(I)+.5)*delx, 2) + k + 1
  + 2/pi * (sum (J, w(J) * ((ord(J)-0.5-ord(I))*delx) *
        log(abs(ord(J)-.5-ord(I))*delx) * (p(J+1)-p(J-1)) / 2)
        + w0 * (-0.5-ord(I)) * delx * log((ord(I)+.5)*delx) * p("1") / 2) )
- ( power(xa + (ord(I)-.5)*delx, 2) + k + 1
  + 2/pi * (sum (J, w(J) * ((ord(J)+0.5-ord(I))*delx) *
        log(abs(ord(J)+.5-ord(I))*delx) * (p(J+1)-p(J-1)) / 2)
        + w0 * (+0.5-ord(I)) * delx * log((ord(I)-.5)*delx) * p("1") / 2) )
)

        =g=

(1/power(delx,2)) * (
(p(I+1)-p(I)) / exp(alpha*(p(I+1)+p(I))*.5) * power(
( power(xa + (ord(I)+.5)*delx, 2) + k + 1
  + 2/pi * (sum (J, w(J) * ((ord(J)-0.5-ord(I))*delx) *
        log(abs(ord(J)-.5-ord(I))*delx) * (p(J+1)-p(J-1)) / 2)
        + w0 * (-0.5-ord(I)) * delx * log((ord(I)+.5)*delx) * p("1") / 2)
), 3)
-
(p(I)-p(I-1)) / exp(alpha*(p(I)+p(I-1))*.5) * power(
( power(xa + (ord(I)-.5)*delx, 2) + k + 1
  + 2/pi * (sum (J, w(J) * ((ord(J)+0.5-ord(I))*delx) *
        log(abs(ord(J)+.5-ord(I))*delx) * (p(J+1)-p(J-1)) / 2)
        + w0 * (+0.5-ord(I)) * delx * log((ord(I)-.5)*delx) * p("1") / 2)
), 3)
);

model ehl       / psum.k, reynolds.p /;

option limrow = 0;
option limcol = 0;
* option iterlim = 10000;
* option reslim = 2400;

* set problem parameters
alpha = alpha0 * sqrt(load/load0);
lambda = lambda0 * (speed/speed0) / sqr(load/load0);
klevel = 1.6;

* calculate "modified Hertzian solution" using gap as x values
* use gap as temporary var
gap(J) = xa + delx*ord(J);
plevel(J) = (-0.02*xa + gap(J)*.02)$(gap(J) lt -1)
        + (sqrt(1-gap(J)*gap(J)))$(abs(gap(J)) lt 1);

k.l = klevel;
p.l(J) = plevel(J);

solve ehl using mcp;
abort$[ehl.modelstat <> 1] "No solution found";

gap(I) = power(xa + (ord(I)-.5)*delx, 2) + k.l + 1
  + 2/pi * [sum {J, w(J) * ((ord(J)+0.5-ord(I))*delx) *
                    log(abs(ord(J)+.5-ord(I))*delx) * (p.l(J+1)-p.l(J-1)) / 2}
            + w0 * (+0.5-ord(I)) * delx * log((ord(I)-.5)*delx) * p.l("1") / 2];

pressureThickness(J0,'x') = xa + delx * (ord(J0) - 1);
pressureThickness(J,'pressure') = p.L(J);
pressureThickness('0','pressure') = EPS;
pressureThickness(J0,'x') = pressureThickness(J0,'x');
pressureThickness(J,'thickness') = gap(J);
pressureThickness('0','thickness') = gap('1') - [gap('2') - gap('1')];
