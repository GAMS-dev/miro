$title Alternative ramp rate calculation

parameters tempUp, tempDown;

* Given by dan in Active Power Ramp Rates document
parameters u11 /0.006038/, u12 /-0.000003840/, 
           u21 /0.004573/, u22 /-0.0000009099/,
           d11 /0.006783/, d12 /-0.000004314/,
           d21 /0.005138/, d22 /-0.000001022/;


nameplateHigh(gen) = 1$(Pmax(gen) >= 500);

* Dan's document gives ramp rates in MW/min, so rescale
tempUp(gen) = 60 * Pmax(gen) *
                    ((u11 + Pmax(gen)*u12)$(nameplateHigh(gen) eq 0)
                    +(u21 + Pmax(gen)*u22)$(nameplateHigh(gen) eq 1));
tempDown(gen) = 60 * Pmax(gen) *
                      ((d11 + Pmax(gen)*d12)$(nameplateHigh(gen) eq 0)
                      +(d21 + Pmax(gen)*d22)$(nameplateHigh(gen) eq 1));
UWRampUp(gen) = tempUp(gen);
UWRampUp10(gen) = tempUp(gen)/6;
UWRampUp30(gen) = tempUp(gen)/2;
UWRampUpreactive(gen) = inf;
UWRampDown(gen) = tempDown(gen);
UWRampDown10(gen) = tempDown(gen)/6;
UWRampDown30(gen) = tempDown(gen)/2;
UWRampDownreactive(gen) = inf;



