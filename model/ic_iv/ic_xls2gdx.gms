$title "Data conversion utility for ACOPF models"
$if not set in $exit
$setnames "%in%" inpath inname inextension
$setnames "%gams.i%" filepath filename fileextension
$if not set out $set out "%inpath%%inname%.gdx"

$if exist "%out%" $call 'rm "%out%"'
$onecho > recommands.txt
input = "%in%"
output = "%out%"
Trace = 3
MaxDupeErrors = 100

*---------------------------------------------------------------------------------------------------------------------
* Make the table of contents
*---------------------------------------------------------------------------------------------------------------------

Squeeze=N par=ic_gen rng=gen!A1:C5000 Cdim=1 Rdim=1
Squeeze=N par=ic_bus rng=bus!A1:C5000 Cdim=1 Rdim=1

$offecho
execute 'gdxxrw @recommands.txt';



