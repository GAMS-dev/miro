scalar f /100/;

$echo "*asd***bla**" > test.md

$onMultiR
$gdxin data.gdx
$load f
$gdxin
$ifthenE f==99
$echo "*uiuiui*" > test.md
$endif
