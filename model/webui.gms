* webui.gms
$if not set GMSWEBUI $exit
$setNames "%gams.input%" fp fn fe

$if %GMSWEBUI%==0 $exit
$if %GMSWEBUI%==1 $goto runwebui
$ife %GMSWEBUI%>1 $goto mkwebui
$abort Unexpected setting of GMSWEBUI=%GMSWEBUI%

$label runwebui
$batinclude %fn%_webui
$exit

$label mkwebui
$gdxout %fn%_webui
$unload
$gdxout

$set UIInput  UIInput:
$set UIOutput UIOutput:
$ifthene.a %GMSWEBUI%>2
$ifthen.b dExist %gams.sysdir%GMSWebUI
$  set WEBUIDIR %gams.sysdir%GMSWebUI
$else.b
$  set WEBUIDIR ..%system.dirsep%..
$  echo library('methods');if(!"shiny"%in%installed.packages()){install.packages("shiny",repos="https://cloud.r-project.org",dependencies=TRUE)};shiny::runApp(launch.browser=TRUE) > runapp.R
$endif.b
$endif.a
$onecho > writecsv.py
def extractSymText(text, leavehash=0):
   input_tag = '%UIInput%'
   uii = len(input_tag)
   output_tag = '%UIOutput%'
   uio = len(output_tag)
   if text.startswith(input_tag):
      text = text[uii:]
   elif text.startswith(output_tag):
      text = text[uio:]
   if (leavehash==0) and (text.find(' ###')>=0):
      text = text[:text.find(' ###')]
   return text.strip()   

def getCSVHeader(sym):
   if sym.dimension==1:
      if sym.domains_as_strings[0] == '*':
         return 'key,'+sym.name
      else:
         return extractSymText(sym.domains[0].text) + ',' + sym.name
   else:
      domdict = {}
      d_list = []
      for d in sym.domains[:-1]:
         s = extractSymText(d.text)
         if s in domdict:
            d_list.append(s + str(domdict[s]))
            domdict[s] = domdict[s]+1
         else:
            domdict[s] = 1
            d_list.append(s)
      d_list += [ r.key(0) for r in sym.domains[-1] ]
      return ','.join(d_list)
def writeCSVParam(sym, gdxname='none'):
   if gdxname=='none':
      with open(sym.name.lower()+'.csv', 'w') as f:
         if sym.dimension==1:
            f.write(getCSVHeader(sym)+'\n')
            for r in sym:
               f.write(r.key(0) + ',' + str(r.value) + '\n')
         else:
            dbX = sym.database.workspace.add_database(source_database=sym.database)
            p = dbX.add_set('project',sym.dimension-1)
            for r in sym:
               try:
                 p.add_record(r.keys[:-1])
               except:
                 pass
                 
            r_list = [ r.key(0) for r in sym.domains[-1] ]
            f.write(getCSVHeader(sym)+'\n')
            for r in p:
               p_list = r.keys
               for t in r_list:
                  try:
                    p_list.append(str(sym.find_record(r.keys + [t]).value))
                  except:
                    p_list.append('0')
               f.write(','.join(p_list)+'\n')
         f.closed
   else:
      import subprocess
      subprocess.call('gdxdump ' + gdxname + ' epsout=0 symb='+sym.name+' cdim=y format=csv header="' + getCSVHeader(sym) + '" > ' + sym.name.lower()+'.csv', shell=True)
$offecho
$if not set WEBUICONF $set WEBUICONF
$onEmbeddedCode Python:
import sys
sys.path.append('.')
from writecsv import *

rmfiles = ['writecsv.py','%fn%_webui.gdx']
gams.wsWorkingDir = '.'   
ws = gams.ws
input_tag = '%UIInput%'
output_tag = '%UIOutput%'
inc = '%fn%_webui.gms'
s_webuiconf = '%WEBUICONF%'

db = ws.add_database_from_gdx('%fn%_webui.gdx')
input_sym = []
scalar_input_sym = []
output_sym = []
scalar_output_sym = []
domsets = set()
for sym in db:
   for d in range(sym.dimension):
      if sym.domains_as_strings[d] != '*':
         domsets.add(sym.domains_as_strings[d])
input_domsets = set()
for s in domsets:
   if db[s].text.startswith(input_tag):
      input_domsets.add(s)

for sym in db:
   if sym.text.startswith(input_tag):
      if (sym.dimension>0) and (type(sym)==GamsParameter):
         input_sym.append(sym.name) 
      elif (sym.dimension==0) and (type(sym)==GamsParameter):
         scalar_input_sym.append(sym.name)
      elif (sym.dimension==1) and (type(sym)==GamsSet) and (sym.name not in domsets):
         scalar_input_sym.append(sym.name)
      elif (sym.dimension==1) and (type(sym)==GamsSet) and (sym.name in input_domsets):
         pass
      else:
         raise Exception('Unhandled external input symbol ' + sym.name)

   if sym.text.startswith(output_tag):
      if (sym.dimension>0) and (type(sym)==GamsParameter):
         output_sym.append(sym.name) 
      elif (sym.dimension==0) and (type(sym)==GamsParameter):
         scalar_output_sym.append(sym.name) 
      elif (sym.dimension==1) and (type(sym)==GamsSet):
         scalar_output_sym.append(sym.name) 
      elif (sym.dimension==1) and (type(sym)==GamsParameter):
         scalar_output_sym.append(sym.name) 
      else:
         raise Exception('Unhandled external output symbol ' + sym.name)

SOhidden = True
SOtrueLen = 0
for s in scalar_output_sym:
   if s not in domsets:
      SOtrueLen = SOtrueLen+1
      text = extractSymText(db[s].text,1)
      if text.find(' ###')==-1:
         SOhidden = False

# Create data reading batinclude file
with open(inc, 'w') as f:
   if len(input_sym)>0:
      f.write('$onExternalInput\n$kill')
      doms = set()
      for s in input_sym:
         f.write(' '+s)
         for d in db[s].domains_as_strings[:-1]:
            if (d!='*') and db[d].text.startswith(input_tag):
               doms.add(d)
      for s in doms:
         f.write(' '+s)
      f.write('\n*$killUel\n$offExternalInput\n$offdigit\n')
   
   if len(scalar_input_sym)>0:
      f.write('$batInclude loadCSV scalars\n')
   for s in input_sym:
      f.write('$batInclude loadCSV '+s.lower())
      for d in db[s].domains_as_strings[:-1]:
         if (d!='*') and db[d].text.startswith(input_tag):
            f.write(' '+d)
      f.write('\n')
   f.write('$onmulti\n')
   for s in scalar_input_sym:
      if type(db[s])==GamsParameter:
         f.write('$if setenv ' + '%fn%'.upper() + '_' + s.upper() + ' Scalar ' + s + ' / %sysEnv.' + '%fn%'.upper() + '_' + s.upper() + '% /\n')
      if type(db[s])==GamsSet:
         f.write('$if setenv ' + '%fn%'.upper() + '_' + s.upper() + ' Singleton set ' + s + '(' + db[s].domains_as_strings[0] + ') / %sysEnv.' + '%fn%'.upper() + '_' + s.upper() + '% /\n')
   f.write('$offmulti\n$ondigit\n')

   f.write('$onuni\nalias (*')
   for i in range(20):
      f.write(',webui'+str(i)+'_')
   f.write(');\n');
   for i in range(19):
      f.write('set webuis'+str(i+1)+'_(webui0_')
      for j in range(i):
         f.write(',webui'+str(j+1)+'_')
      f.write(');\n');
   for s in output_sym:
      dim = db[s].dimension
      if (db[s].dimension>1) and (db[s].domains_as_strings[-1]!='*'):
         f.write('webuis'+str(dim-1)+'_(webui0_')
         for i in range(dim-2):
            f.write(',webui'+str(i+1)+'_')
         f.write(') = sum(webui'+str(db[s].dimension-1)+'_, ' + s + '(webui0_')
         for i in range(dim-1):
            f.write(',webui'+str(i+1)+'_')
         f.write('));\n'+s+'(webuis'+str(dim-1)+'_,'+db[s].domains_as_strings[-1]+') = '+s+'(webuis'+str(dim-1)+'_,'+db[s].domains_as_strings[-1]+') + eps;\n')
         f.write('option clear=webuis'+str(dim-1)+'_;\n')
      
   f.write('execute_unload "gmswebui.gdx";\n')
   f.write('embeddedCode Python:\n')
   with open("writecsv.py", "r") as myfuncs:
       for line in myfuncs:
          f.write(line)
   f.write('\ngams.wsWorkingDir = "."\n')
   f.write('db = gams.ws.add_database_from_gdx("gmswebui.gdx")\n')
   for s in output_sym:
      f.write('writeCSVParam(db["' + s + '"],"gmswebui.gdx")\n')
   if SOtrueLen>0:
      f.write("with open('scalars_out.csv', 'w') as f:\n");
      f.write("   f.write('Scalar,Description,Value\\n')\n")
      for s in scalar_output_sym:
         if type(db[s])==GamsParameter:
            f.write("   f.write('" + s + ",\"" + extractSymText(db[s].text,1) + "\",'")
            f.write(" + str(db['" + s + "'].first_record().value) + '\\n')\n")
         if (type(db[s])==GamsSet) and (s not in domsets):
            f.write("   if(len(db['" + s + "'])):\n")
            f.write("      f.write('" + s + ",\"" + extractSymText(db[s].text,1) + "\",'")
            f.write(" + str(db['" + s + "'].first_record().key(0)) + '\\n')\n")
      f.write("   f.closed\n")
   f.write("db.__del__()\n")
   f.write("endEmbeddedCode\n")
   f.write("execute 'rm -f gmswebui.gdx';\n")
   f.closed

# Create example input and output reporting batinclude file
import os
s_env = []
for k, v in os.environ.items():
   if k.startswith('%fn%'.upper()+'_'):
      s = k[len('%fn%')+1:]
      s_env.append((s,str(v)))
      
if len(scalar_input_sym)+len(s_env)>0:
   with open('scalars.csv', 'w') as f:
      f.write('Scalar,Description,Value\n')
      for s in scalar_input_sym:
         f.write(s + ',' + extractSymText(db[s].text) + ',')
         if type(db[s])==GamsParameter:
            f.write(str(db[s].first_record().value)+'\n')
         if type(db[s])==GamsSet:
            f.write(db[s].first_record().key(0)+'\n')
      for s in s_env:
         f.write(s[0] + ',' + s[0] + ',' + s[1] + '\n')
      f.closed
if SOtrueLen>0:
   rmfiles.append('scalars_out.csv')
   with open('scalars_out.csv', 'w') as f:
      f.write('Scalar,Description,Value\n')
      for s in scalar_output_sym:
         if type(db[s])==GamsParameter:
            f.write(s + ',' + extractSymText(db[s].text) + ',255+\n')
         if (type(db[s])==GamsSet) and (s not in domsets):
            f.write(s + ',' + extractSymText(db[s].text) + ',' + db[db[s].domains_as_strings[0]].first_record().key(0)+'\n')
      f.closed

for s in input_sym:
   writeCSVParam(db[s])

for s in output_sym:
   rmfiles.append(s.lower()+'.csv')
   with open(s.lower() + '.csv', 'w') as f:
      f.write(getCSVHeader(db[s])+'\n')
      d_list  = [ d.first_record().key(0) for d in db[s].domains[:-1] ]
      d_list += [ '0' for r in db[s].domains[-1] ]
      f.write(','.join(d_list)+'\n')
      f.closed

# Create example XLSX file
def is_number(s):
  try:
    float(s)
    return True
  except ValueError:
    return False

try:
   import csv
   from xlsxwriter.workbook import Workbook
       
   workbook = Workbook('%fn%.xlsx')
   
   slist = list(input_sym)
   if len(scalar_input_sym):
      slist.append('scalars')
   slist += output_sym
   if SOtrueLen>0:
      slist.append('scalars_out')
   
   for s in slist:
     worksheet = workbook.add_worksheet(s) #worksheet with symbol name
     with open(s.lower() + '.csv', 'r') as f:
       reader = csv.reader(f)
       for r, row in enumerate(reader):
         for c, col in enumerate(row):
           if is_number(col):
             worksheet.write(r, c, float(col)) #write the csv file content into it
           else:
             worksheet.write(r, c, col)
   workbook.close()
except:
   pass   

import copy
def dict_merge(a, b):
    '''recursively merges dict's. not just simple a['key'] = b['key'], if
    both a and bhave a key who's value is a dict then dict_merge is called
    on both values and the result stored in the returned dictionary.'''
    if not isinstance(b, dict):
        return b
    result = copy.deepcopy(a)
    for k, v in b.items():
        if k in result and isinstance(result[k], dict):
                result[k] = dict_merge(result[k], v)
        else:
            result[k] = copy.deepcopy(v)
    return result
    
import json
config = { "pageTitle" : "%system.title%",
           "gamsMetaDelim" : "###",
           "gamsWEBUISwitch" : "--GMSWEBUI=1",
           "fileExchange" : "csv",
           "csvDelim" : "," }

io_dict = {}           
for s in input_sym:
   text = extractSymText(db[s].text,1)
   if text.find(' ###')>=0:
      e_dict = json.loads(text[text.find(' ###')+4:])
   else:   
      e_dict = {}
   headers = {}
   for d in db[s].domains[:-1]:
      headers[extractSymText(d.text)] = { 'type':'set' }
   for r in db[s].domains[-1]:
      headers[r.key(0).lower()] = { 'type':'parameter' }
   auto = { 'alias':extractSymText(db[s].text), 'headers':headers }
   io_dict[s.lower()] = dict_merge(auto,e_dict)

needScalar = False
for s in scalar_input_sym:
   text = extractSymText(db[s].text,1)
   if text.find(' ###')>=0:
      e_dict = json.loads(text[text.find(' ###')+4:])
   else:   
      e_dict = {}
   auto = { 'alias':extractSymText(db[s].text) }
   add2Dict = True
   if 'slider' in e_dict:
      auto['slider'] = {'label':extractSymText(db[s].text)}
   elif 'daterange' in e_dict:
      auto['daterange'] = {'label':extractSymText(db[s].text)}
   elif 'dropdown' in e_dict:
      auto['dropdown'] = {'label':extractSymText(db[s].text)}
   else:
      add2Dict = False
   if add2Dict:
      io_dict[s.lower()] = dict_merge(auto,e_dict)
   else:
      needScalar = True;

if needScalar:      
   io_dict['scalars'] = { 'alias':'Scalars', 'headers':{'Scalar':{'type':'set'},'Description':{'type':'acronym'},'Value':{'type':'acronym'}} }
if len(s_webuiconf):
   config['gamsInputFiles'] = dict_merge(io_dict,json.loads(s_webuiconf))
elif os.path.isfile('webuiconf.json'):
   with open('webuiconf.json', 'r') as jffile:
      config['gamsInputFiles'] = dict_merge(io_dict,json.load(jffile))
else:
   config['gamsInputFiles'] = io_dict
   
io_dict = {}           
for s in output_sym:
   text = extractSymText(db[s].text,1)
   if text.find(' ###')>=0:
      e_dict = json.loads(text[text.find(' ###')+4:])
   else:   
      e_dict = {}
   headers = {}
   if db[s].dimension==1:
      if db[s].domains_as_strings[0] == '*':
         io_dict[s.lower()] = dict_merge({ 'alias':extractSymText(db[s].text), 'headers':{'key':{'type':'set'},db[s].name:{'type':'parameter'}} }, e_dict)
      else:
         io_dict[s.lower()] = dict_merge({ 'alias':extractSymText(db[s].text), 'headers':{extractSymText(db[s].text):{'type':'set'},db[s].name:{'type':'parameter'}} }, e_dict)
   else:         
      headers = {}
      for d in db[s].domains[:-1]:
         headers[extractSymText(d.text)] = { 'type':'set' }
      for r in db[s].domains[-1]:
         headers[r.key(0).lower()] = { 'type':'parameter' }
      auto = { 'alias':extractSymText(db[s].text), 'headers':headers }
      io_dict[s.lower()] = dict_merge(auto,e_dict)

if SOtrueLen>0:
   if SOhidden:
      io_dict['scalars_out'] = { 'alias':'Scalars', 'hidden':True, 'count':len(scalar_output_sym), 'headers':{'Scalar':{'type':'set'},'Description':{'type':'acronym'},'Value':{'type':'acronym'}} }
   else:
      io_dict['scalars_out'] = { 'alias':'Scalars', 'count':len(scalar_output_sym), 'headers':{'Scalar':{'type':'set'},'Description':{'type':'acronym'},'Value':{'type':'acronym'}} }
config['gamsOutputFiles'] = io_dict

#json.dump(config, sys.stdout, indent=4, sort_keys=False)
try:
   if not os.path.exists('./conf/'):
      os.makedirs('./conf/')
except OSError:
   print ('Error: Creating directory: conf')
with open('conf/GMSIO_config.json', 'w') as f:
   json.dump(config, f, indent=4, sort_keys=False)
db.__del__()
import os
for s in rmfiles:
   os.remove(s.lower())
   
if %GMSWEBUI%>2 and os.name == "nt":
    import winreg
    import re
    
    def get_r_path():
        def major_minor_micro(version):
            major, minor, micro = re.search('(\d+)\.(\d+)\.(\d+)', version).groups()
            return int(major), int(minor), int(micro)
        try:
            aReg = winreg.ConnectRegistry(None,winreg.HKEY_LOCAL_MACHINE)
            aKey = winreg.OpenKey(aReg, r"SOFTWARE\R-Core\R")
        except FileNotFoundError:
            return("")
        paths = []
        for i in range(20):
            try:
                asubkey_name = winreg.EnumKey(aKey,i)
                asubkey = winreg.OpenKey(aKey,asubkey_name)
                paths.append(winreg.QueryValueEx(asubkey, "InstallPath")[0])
            except EnvironmentError:
                break
        if len(paths) == 0:
           return("")
        latestRPath = max(paths, key=major_minor_micro)
        latestR = major_minor_micro(latestRPath)
       
        if latestR[0] < 3 or latestR[0] == 3 and latestR[1] < 5:
          print('test')
          os.environ["PYEXCEPT"] = "RVERSIONERROR"
          raise FileNotFoundError('Bad R version')
        return latestRPath + os.sep + "bin" + os.sep
    os.environ["RPATH"] = get_r_path()
    if os.path.exists(r"%gams.sysdir%GMSWebUI"):
        sysdir = r"%gams.sysdir% ".strip().replace("\\","\\\\")
        with open("runapp.R", "w") as f: 
           f.write("library('methods')\n")
           f.write(".libPaths(c('" + sysdir + "library', .libPaths()))\n")
           f.write("if(!'shiny'%in%installed.packages()){\n")
           f.write("install.packages('shiny',repos='https://cloud.r-project.org',dependencies=TRUE)}\n")
           f.write("shiny::runApp(launch.browser=TRUE)")
    else:
        with open("runapp.R", "w") as f: 
           f.write("library('methods')\n")
           f.write("if(!'shiny'%in%installed.packages()){\n")
           f.write("install.packages('shiny',repos='https://cloud.r-project.org',dependencies=TRUE)}\n")
           f.write("shiny::runApp(launch.browser=TRUE)")
elif %GMSWEBUI%>2:
    with open("runapp.R", "w") as f: 
           f.write("library('methods')\n")
           f.write("if(!'shiny'%in%installed.packages()){\n")
           f.write("install.packages('shiny',repos='https://cloud.r-project.org',dependencies=TRUE)}\n")
           f.write("shiny::runApp(launch.browser=TRUE)")
$offembeddedCode
$hiddencall rm -rf __pycache__
$ifthen not errorfree
$if %sysenv.PYEXCEPT% == "RVERSIONERROR" $abort The R version you have installed is too old. Plase install R 3.5 or higher.
$terminate
$endif
$ifthene %GMSWEBUI%>2
$  call cd "%WEBUIDIR%" && "%sysenv.RPATH%Rscript" "%fp%runapp.R" -modelPath="%fp%%fn%%fe%" -gamsSysDir="%gams.sysdir%"
$  if errorlevel 1 $abort Problems executing GMS WebUI. Make sure you have a valid WebUI installation.
$endif
$terminate
