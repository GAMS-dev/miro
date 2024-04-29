$onExternalInput
set test1 /i1*i100/
test2 /j1*j100/
test3 /k1*k100/
test4 /l1*l100/
test5 /m1*m100/
test6 /n1*n100/;
$offExternalInput

$onExternalOutput
Parameter out1(test1) /i1 100, i2 150/
out2(test2) /j1 123, j10 456/
out3(test1) /i1 100, i2 150/
out4(test6) /n1 1.234, n99 2.345/
out5(test5) /m10 10, m20 20, m30 30/;
$offExternalOutput
