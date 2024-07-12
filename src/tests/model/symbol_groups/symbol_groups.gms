$onExternalInput
set test1 /i1*i100, asdefghijklmnopqrstuvwxyzasdefghijklmnopqrstuvwxyz/
test2 /j1*j100/
test3 /k1*k100/
test4 /l1*l100/
test5 /m1*m100/
test6 /n1*n100/;
$offExternalInput

$onExternalOutput
Parameter out1(test1, test2, test3, test4, test5) /asdefghijklmnopqrstuvwxyzasdefghijklmnopqrstuvwxyz.j1.k1.l1.m1 100, i2.j2.k2.l2.m2 150, i3.j3.k3.l3.m3 160, i4.j4.k4.l4.m4 170, i5.j5.k5.l5.m5 180, i6.j6.k6.l6.m6 190, i7.j7.k7.l7.m7 200/
out2(test2) /j1 123, j10 456/
out3(test1) /i1 100, i2 150/
out4(test6) /n1 1.234, n99 2.345/
out5(test5) /m10 10, m20 20, m30 30/;
$offExternalOutput
