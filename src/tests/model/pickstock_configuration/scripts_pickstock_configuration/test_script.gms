$ifThen "%testVar%"=="test"
$onEmbeddedCode Python:
from gams import GamsWorkspace
import os
import sys
cwd = os.getcwd()

if len(sys.argv) > 1:
    ws = GamsWorkspace(system_directory = sys.argv[1])
else:
    ws = GamsWorkspace()

file1 = open('hcube_file_names.txt', 'r')
lines = file1.read().splitlines()


f= open("out.txt","w+")

for line in lines:
  db = ws.add_database_from_gdx(os.path.join(cwd, line))
  maxstock = db["maxstock"].first_record().value
  trainingdays = db["trainingdays"].first_record().value
  solver = [ rec.keys[0] for rec in db["solver"] ]
  f.write("<div id='" + line[:-4] + "' style='padding-bottom:25px;'>")
  f.write("<p id='scenario', style='color:red'>Scenario: " + line[:-4] + "</p>")
  f.write("<p id='maxstock', style='color:red'>maxstock: " + str(maxstock) + "</p>")
  f.write("<p id='trainingdays', style='color:red'>trainingdays: " + str(trainingdays) + "</p>")
  f.write("<p id='solver', style='color:red;'>solver: " + str(solver) + "</p>")
  f.write("</div>")
f.close()
$offEmbeddedCode

$endif
$ifThen not "%testVar%"=="test"
File out /out.txt/;
put out "No --test variable set to 'test' via analysis script!"
putclose out;
$endif
