$title Compute derived line limits

* Derived parameters for estimating line limits
parameters line_v, num_c, c_size, D_S, D_SL, D_eq,
           length, b_temp, SIL, Pmax_tot;
parameter d "Distance between conductors" /0.4572/;
table typical_SIL(*,*)
    V_rated   SIL_min   SIL_max
1    69          12      13
2    138         47      52  
3    230         134     145
4    345         325     425
5    500         850     1075
6    765         2200    2300
;

* Give line limits an upper bound of the maximum total generator output
Pmax_tot = sum(gen$status(gen), Pmax(gen));

* Fill in any missing or invalid parameters
baseKV(bus)$(baseKV(bus) eq 0) = 230;

* If line_v = 0, then there's a transformer (or no line)
line_v(from,to,c)$(line(from,to,c) and (baseKV(from) eq baseKV(to))) = baseKV(to);

* Calculate line length
num_c(from,to,c)$(line_v(from,to,c)) = 1
                    + 1$(line_v(from,to,c) ge 300)
                    + 1$(line_v(from,to,c) ge 400)
                    + 1$(line_v(from,to,c) ge 600)
;

c_size(from,to,c)$(line_v(from,to,c)) =
                      0.795$(line_v(from,to,c) le 200)
                    + 0.954$(line_v(from,to,c) gt 200)
;

D_S(from,to,c)$(line_v(from,to,c)) =
                      0.01143$(c_size(from,to,c) eq 0.795)
                    + 0.01228$(c_size(from,to,c) eq 0.954)
;

D_SL(from,to,c)$(line_v(from,to,c)) =
                    (1 + 0.091$(num_c(from,to,c) eq 4))
                    * (D_S(from,to,c) * d**(num_c(from,to,c) - 1))
                        ** (1/(num_c(from,to,c)))
;

* 0.3048 m/ft and enforce minimum of 1 ft distance
D_eq(from,to,c)$(line_v(from,to,c)) =
                    0.077*max((line_v(from,to,c) - 3.11) * 0.3048,
                        0.3048)
;

* Sometimes x is given as less than 0 -- using abs for now
length(from,to,c)$(line_v(from,to,c)) =
    abs(branchinfo(from,to,c,'x','given')) * sqr(line_v(from,to,c)) * 10**7
    / (10**3 * 2 * 2 * pi * 60 * baseMVA * log(D_eq(from,to,c) / D_SL(from,to,c)))
;

* Calculate SIL
b_temp(from,to,c)$(line_v(from,to,c)) = branchinfo(from,to,c,'bc','given');
b_temp(from,to,c)$(line_v(from,to,c) and (b_temp(from,to,c) eq 0)) = 0.01;
* Some of the b entries have been 0, but we're ignoring that and taking an abs

SIL(from,to,c)$(line_v(from,to,c)) = 
                baseMVA * sqrt(abs(b_temp(from,to,c)))
                    / sqrt(sqrt(sqr(branchinfo(from,to,c,'x','given')) + sqr(branchinfo(from,to,c,'r','given'))))
;

* Sanity check on SIL -- if it's out of range, replace with
*                        midpoint and set a flag
set SIL_out_of_range, SIL_in_range;
parameter SIL_error;
set s /1*6/;
loop((from,to,c)$line_v(from,to,c),
    loop(s$((abs(line_v(from,to,c) - typical_SIL(s,'V_rated')) le 30)
                             and ((SIL(from,to,c) < typical_SIL(s,'SIL_min'))
                               or (SIL(from,to,c) > typical_SIL(s,'SIL_max')))),
        SIL_out_of_range(from,to,c) = yes;
        SIL_error(from,to,c)$(SIL(from,to,c) < typical_SIL(s,'SIL_min')) = SIL(from,to,c) - typical_SIL(s,'SIL_min');
        SIL_error(from,to,c)$(SIL(from,to,c) > typical_SIL(s,'SIL_max')) = SIL(from,to,c) - typical_SIL(s,'SIL_max');
        SIL(from,to,c) = (typical_SIL(s,'SIL_min') + typical_SIL(s,'SIL_max'))/2;
    );
);
SIL_in_range(from,to,c)$(line_v(from,to,c) and not SIL_out_of_range(from,to,c)) = yes;

branchinfo(from,to,c,'rateA','SIL_avg')$line(from,to,c) = branchinfo(from,to,c,'rateA','given');
loop((from,to,c,s)$(line_v(from,to,c)
                and ((abs(line_v(from,to,c) - typical_SIL(s,'V_rated')) le 30))),
    branchinfo(from,to,c,'rateA','SIL_avg')$(line_v(from,to,c)) = 
    min(
        max(min(42.40*((length(from,to,c)*0.621371)**(-0.6595)), 3.0),
            0.5)
         * (typical_SIL(s,'SIL_min')+typical_SIL(s,'SIL_max'))/2
      ,
        Pmax_tot)
);

* If there's a transformer, just take the given limit for now
branchinfo(from,to,c,'rateA','UWCalc')$line(from,to,c) = branchinfo(from,to,c,'rateA','given');

* Calculate line limits
* 0.621371 mi/km
branchinfo(from,to,c,'rateA','UWCalc')$(line_v(from,to,c)) =
    min(
        max(min(42.40*((length(from,to,c)*0.621371)**(-0.6595)), 3.0),
            0.5)
        * SIL(from,to,c)
      ,
        Pmax_tot
    )
;
set limits /given, UWCalc, SIL_avg/;
branchinfo(from,to,c,'rateA','min')$line(from,to,c) = smin(limits, branchinfo(from,to,c,'rateA',limits));
branchinfo(from,to,c,'rateA','max')$line(from,to,c) = smax(limits, branchinfo(from,to,c,'rateA',limits));
branchinfo(from,to,c,'rateA','inf')$line(from,to,c) = inf;

* Approximate short-term and contingency line limits
branchinfo(from,to,c,'rateB','uwcalc')$(line(from,to,c) and (ratio(from,to,c) eq 1)) = 1.20 * branchinfo(from,to,c,'rateA','uwcalc');
branchinfo(from,to,c,'rateB','uwcalc')$(line(from,to,c) and (ratio(from,to,c) ne 1)) = 1.20 * branchinfo(from,to,c,'rateA','uwcalc');
branchinfo(from,to,c,'rateC','uwcalc')$(line(from,to,c) and (ratio(from,to,c) eq 1)) = 1.33 * branchinfo(from,to,c,'rateA','uwcalc');
branchinfo(from,to,c,'rateC','uwcalc')$(line(from,to,c) and (ratio(from,to,c) ne 1)) = 1.25 * branchinfo(from,to,c,'rateA','uwcalc');

