scalar f /100/;

$echo "# test1" > test.md

$onMultiR
$gdxin data.gdx
$load f
$gdxin
$ifthenE f==99
$echo "start" > %testfile%
$call sleep 5
$echo "finish" > %testfile%
$endif
