* webui.gms
$offListing
$if not set MAX_VAL_COL $set MAX_VAL_COL -1
$if not set WEBUI $exit
$setNames "%gams.input%" fp fn fe

$if %WEBUI%==1 $goto runwebui
$if %WEBUI%=="launch" $goto mkwebui
$if %WEBUI%=="compile" $goto mkwebui
$abort Unexpected setting of WEBUI=%WEBUI%. Please use either "launch" to launch the WebUI or "compile" to compile configuration files. 

$label runwebui
$batinclude %fn%_webui
$exit

$label mkwebui
$gdxout %fn%_webui
$unload
$gdxout

$if not set appLogoPath $set appLogoPath ""
$ifthen not set mkApp
$   set mkApp 0
$else
$   set mkApp 1
$endif

$set UIInput  UIInput:
$set UIOutput UIOutput:
$ifthen dExist %gams.sysdir%GMSWebUI
$  set WEBUIDIR %gams.sysdir%GMSWebUI
$else
$  set WEBUIDIR %fp%..%system.dirsep%..
$endif
$onecho > writecsv.py
from subprocess import run
from shlex import quote

def extractSymText(sym, leavehash=0):
   text = sym.text
   aliasIdentifier = 'Aliased with '
   if text.startswith(aliasIdentifier):
      aliasedSet = text[len(aliasIdentifier):]
      try:
         db[aliasedSet].text
      except:
         pass
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
   text = text.strip()
   if(len(text) == 0):
      text = sym.name
   return text

def expandLastCol(sym, max_val_col):
   if (sym.domains[-1].name.lower().endswith('hdr') or
        sym.domains[-1].text.lower().endswith('hdr')):
      expand_last_col = True
   else:
      expand_last_col = len(sym.domains[-1]) <= max_val_col
   return expand_last_col

def getCSVHeader(sym, max_val_col):
   expand_last_col = expandLastCol(sym, max_val_col)
   
   if sym.dimension==1:
      if sym.domains_as_strings[0] == '*':
         return 'key,'+extractSymText(sym)
      else:
         return extractSymText(sym.domains[0]) + ',' + extractSymText(sym)
   else:
      domdict = {}
      d_list = []
      if expand_last_col:
         domains = sym.domains[:-1]
      else:
         domains = sym.domains
      for d in domains:
         s = extractSymText(d)
         if s in domdict:
            d_list.append(s + str(domdict[s]))
            domdict[s] = domdict[s]+1
         else:
            domdict[s] = 1
            d_list.append(s)
      if expand_last_col:
         d_list += [ r.key(0) for r in sym.domains[-1] ]
      else:
         s = extractSymText(sym)
         d_list.append(s)
      return ','.join(d_list)

      
def writeCSVParam(sym, gdxname='none', max_val_col = 5, isGamsSet = False):
   expand_last_col = expandLastCol(sym, max_val_col)
   if gdxname=='none':
      with open(sym.name.lower()+'.csv', 'w') as f:
         if sym.dimension==1:
            f.write(getCSVHeader(sym, max_val_col)+'\n')
            if(isGamsSet):
               for r in sym:
                  f.write(r.key(0) + '\n')
            else:
               for r in sym:
                  f.write(r.key(0) + ',' + str(r.value) + '\n')
         else:
            dbX = sym.database.workspace.add_database(source_database=sym.database)
            if expand_last_col:
               dim = sym.dimension-1
            else:
               dim = sym.dimension
            p = dbX.add_set('project',dim)
            for r in sym:
               try:
                 if expand_last_col:
                    p.add_record(r.keys[:-1])
                 else:
                    p.add_record(r.keys)
               except:
                 pass
            if expand_last_col:
               r_list = [ r.key(0) for r in sym.domains[-1] ]
            else:
               r_list = [ extractSymText(sym) ]
            f.write(getCSVHeader(sym, max_val_col)+'\n')
            #rows (no values)
            for r in p:
               p_list = r.keys
               #Values
               for t in r_list:
                  try:
                     if expand_last_col:
                        p_list.append(str(sym.find_record(r.keys + [t]).value))
                     else:
                        p_list.append(str(sym.find_record(r.keys).value))
                  except:
                    p_list.append('0')
               f.write(','.join(p_list)+'\n')
         f.closed
   else:
      if expand_last_col:
         cdim = 'y'
      else:
         cdim = 'n'

      run('gdxdump ' + quote(gdxname) + ' epsout=0 symb='+quote(sym.name)+' cdim='+quote(cdim)+' format=csv header="' + getCSVHeader(sym, %MAX_VAL_COL%) + '" > ' + quote(sym.name.lower())+'.csv', shell=True)
$offecho
$if not set WEBUICONF $set WEBUICONF
$onEmbeddedCode Python:
from sys import path
from re import search
path.append('.')

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
scalar_output_type = []
scalar_output_text = []
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
         if search(r'["\']multiple["\']\s*:\s*true',sym.text):
            input_sym.append(sym.name)
         else:
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
         scalar_output_type.append('parameter')
         scalar_output_text.append(extractSymText(sym))
      elif (sym.dimension==1) and (type(sym)==GamsSet):
         scalar_output_sym.append(sym.name)
         scalar_output_type.append('set')
         scalar_output_text.append(extractSymText(sym))
      elif (sym.dimension==1) and (type(sym)==GamsParameter):
         scalar_output_sym.append(sym.name)
         scalar_output_type.append('parameter')
         scalar_output_text.append(extractSymText(sym))
      else:
         raise Exception('Unhandled external output symbol ' + sym.name)
SOhidden = True
SOtrueLen = 0
for s in scalar_output_sym:
   if s not in domsets:
      SOtrueLen = SOtrueLen+1
      text = extractSymText(db[s],1)
      if text.find(' ###')==-1:
         SOhidden = False

# Create data reading batinclude file
with open(inc, 'w') as f:
   if len(input_sym)>0:
      f.write('$onExternalInput\n$kill')
      doms = set()
      for s in input_sym:
         f.write(' '+s)
         if(expandLastCol(db[s], %MAX_VAL_COL%)):
            domains = db[s].domains_as_strings[:-1]
         else:
            domains = db[s].domains_as_strings
         for d in domains:
            if (d!='*') and db[d].text.startswith(input_tag):
               doms.add(d)
      for s in doms:
         f.write(' '+s)
      f.write('\n*$killUel\n$offExternalInput\n$offdigit\n')
   
   if len(scalar_input_sym)>0:
      f.write('$libInclude loadCSV scalars\n')
   for s in input_sym:
      if(expandLastCol(db[s], %MAX_VAL_COL%)):
         f.write('$libInclude loadCSV '+s.lower())
         for d in db[s].domains_as_strings[:-1]:
            if (d!='*') and db[d].text.startswith(input_tag):
               f.write(' '+d)
         f.write('\n')
      else:
         if type(db[s]) != GamsSet:
            f.write('$setEnv GMSWEBUI_EXPAND_HEADER 1\n')
         
         f.write('$libInclude loadCSV '+s.lower())
         for d in db[s].domains_as_strings:
            if (d!='*') and db[d].text.startswith(input_tag):
               f.write(' '+d)
         if type(db[s]) != GamsSet:
            f.write('\n$dropEnv GMSWEBUI_EXPAND_HEADER\n')
         else:
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
         f.write('));\nloop((webuis'+str(dim-1)+'_,'+db[s].domains_as_strings[-1]+'), '+s+'(webuis'+str(dim-1)+'_,'+db[s].domains_as_strings[-1]+') = '+s+'(webuis'+str(dim-1)+'_,'+db[s].domains_as_strings[-1]+') + eps;break;);\n')
         f.write('option clear=webuis'+str(dim-1)+'_;\n')
      
   f.write('execute_unload "gmswebui.gdx";\n')
   f.write('embeddedCode Python:\n')
   with open("writecsv.py", "r") as myfuncs:
       for line in myfuncs:
          f.write(line)
   f.write('\ngams.wsWorkingDir = "."\n')
   f.write('db = gams.ws.add_database_from_gdx("gmswebui.gdx")\n')
   for s in output_sym:
      f.write('writeCSVParam(db["' + s + '"],"gmswebui.gdx", %MAX_VAL_COL%)\n')
   if SOtrueLen>0:
      f.write("with open('scalars_out.csv', 'w') as f:\n");
      f.write("   f.write('Scalar,Description,Value\\n')\n")
      for s in scalar_output_sym:
         if type(db[s])==GamsParameter:
            f.write("   f.write('" + s + ",\"" + extractSymText(db[s],1) + "\",'")
            f.write(" + str(db['" + s + "'].first_record().value) + '\\n')\n")
         if (type(db[s])==GamsSet) and (s not in domsets):
            f.write("   if(len(db['" + s + "'])):\n")
            f.write("      f.write('" + s + ",\"" + extractSymText(db[s],1) + "\",'")
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
   rmfiles.append('scalars.csv')
   with open('scalars.csv', 'w') as f:
      f.write('Scalar,Description,Value\n')
      for s in scalar_input_sym:
         f.write(s + ',' + extractSymText(db[s]) + ',')
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
            f.write(s + ',' + extractSymText(db[s]) + ',255+\n')
         if (type(db[s])==GamsSet) and (s not in domsets):
            try:
               f.write(s + ',' + extractSymText(db[s]) + ',' + db[db[s].domains_as_strings[0]].first_record().key(0)+'\n')
            except:
               f.write(s + ',' + extractSymText(db[s]) + ',0\n')
      f.closed

for s in input_sym:
   rmfiles.append(s.lower()+'.csv')
   writeCSVParam(db[s], max_val_col = %MAX_VAL_COL%, isGamsSet = type(db[s])==GamsSet)

for s in output_sym:
   rmfiles.append(s.lower()+'.csv')
   with open(s.lower() + '.csv', 'w') as f:
      f.write(getCSVHeader(db[s], %MAX_VAL_COL%)+'\n')
      if(expandLastCol(db[s], %MAX_VAL_COL%)):
         try:
           d_list  = [ d.first_record().key(0) for d in db[s].domains[:-1] ]
         except:
           d_list  = [ '0' for d in db[s].domains[:-1] ]
         d_list += [ '0' for r in db[s].domains[-1] ]
      else:
         try:
            d_list  = [ d.first_record().key(0) for d in db[s].domains ]
         except:
            d_list  = [ '0' for d in db[s].domains]
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
   from xlsxwriter.workbook import Workbook
except:
   try:
      import pip
      if(hasattr(pip, 'main')):
         pip.main(['install', 'xlsxwriter'])
      else:
         pip._internal.main(['install', 'xlsxwriter'])
      from xlsxwriter.workbook import Workbook
   except Exception as e:
      print("WARNING: xlsxwriter could not be installed! No Excel file will be written. Error message: " + str(e))
try:
   import csv
       
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
   
from copy import deepcopy

def dict_merge(a, b):
    '''recursively merges dict's. not just simple a['key'] = b['key'], if
    both a and bhave a key who's value is a dict then dict_merge is called
    on both values and the result stored in the returned dictionary.'''
    if not isinstance(b, dict):
        return b
    result = deepcopy(a)
    for k, v in b.items():
        if k in result and isinstance(result[k], dict):
                result[k] = dict_merge(result[k], v)
        else:
            result[k] = deepcopy(v)
    return result
    
import json
config = { "pageTitle" : "%system.title%",
           "gamsMetaDelim" : "###",
           "gamsWEBUISwitch" : "--WEBUI=1",
           "fileExchange" : "csv",
           "csvDelim" : "," }

io_dict = {}           
for s in input_sym:
   text = extractSymText(db[s],1)
   if text.find(' ###')>=0:
      e_dict = json.loads(text[text.find(' ###')+4:])
   else:   
      e_dict = {}
   
   headers = {}
   if 'dropdown' in e_dict:
      auto = { 'alias':extractSymText(db[s]) }
   else:
      domdict = {}
      if expandLastCol(db[s], %MAX_VAL_COL%):
         for d in db[s].domains[:-1]:
            symText = extractSymText(d)
            if symText in domdict:
               headers[symText + str(domdict[symText])] = { 'type':'set' }
               domdict[symText] = domdict[symText]+1
            else:
               domdict[symText] = 1
               headers[symText] = { 'type':'set' }
      
         for r in db[s].domains[-1]:
            headers[r.key(0)] = { 'type':'parameter' }
      else:
         for d in db[s].domains:
            symText = extractSymText(d)
            if symText in domdict:
               headers[symText + str(domdict[symText])] = { 'type':'set' }
               domdict[symText] = domdict[symText]+1
            else:
               domdict[symText] = 1
               headers[symText] = { 'type':'set' }
      
         headers[extractSymText(db[s])] = { 'type':'parameter' }
      
      auto = { 'alias':extractSymText(db[s]), 'headers':headers }
   io_dict[s.lower()] = dict_merge(auto,e_dict)

needScalar = False
needScalarSymNames = []
needScalarSymTypes = []
needScalarSymText = []
for s in scalar_input_sym:
   text = extractSymText(db[s],1)
   if text.find(' ###')>=0:
      e_dict = json.loads(text[text.find(' ###')+4:])
   else:   
      e_dict = {}
   auto = { 'alias':extractSymText(db[s]) }
   add2Dict = True
   if 'slider' in e_dict:
      auto['slider'] = {'label':extractSymText(db[s])}
   elif 'date' in e_dict:
      auto['date'] = {'label':extractSymText(db[s])}
   elif 'daterange' in e_dict:
      auto['daterange'] = {'label':extractSymText(db[s])}
   elif 'dropdown' in e_dict:
      auto['dropdown'] = {'label':extractSymText(db[s])}
   elif 'chekbox' in e_dict:
      auto['chekbox'] = {'label':extractSymText(db[s])}
   else:
      add2Dict = False
   if add2Dict:
      io_dict[s.lower()] = dict_merge(auto,e_dict)
   else:
      needScalarSymNames.append(s.lower());
      needScalarSymText.append(extractSymText(db[s]));
      if(type(db[s]) == GamsSet):
         needScalarSymTypes.append('set');
      else:
         needScalarSymTypes.append('parameter');
      needScalar = True;

if needScalar:      
   io_dict['scalars'] = { 'alias':'Input Scalars', 'symnames':needScalarSymNames, 'symtext':needScalarSymText, 'symtypes':needScalarSymTypes, 'headers':{'Scalar':{'type':'set'},'Description':{'type':'acronym'},'Value':{'type':'acronym'}} }
if len(s_webuiconf):
   config['gamsInputFiles'] = dict_merge(io_dict,json.loads(s_webuiconf))
elif os.path.isfile('webuiconf.json'):
   with open('webuiconf.json', 'r') as jffile:
      config['gamsInputFiles'] = dict_merge(io_dict,json.load(jffile))
   
else:
   config['gamsInputFiles'] = io_dict
   
io_dict = {}           
for s in output_sym:
   text = extractSymText(db[s],1)
   if text.find(' ###')>=0:
      e_dict = json.loads(text[text.find(' ###')+4:])
   else:   
      e_dict = {}
   headers = {}
   if db[s].dimension==1:
      if db[s].domains_as_strings[0] == '*':
         io_dict[s.lower()] = dict_merge({ 'alias':extractSymText(db[s]), 'headers':{'key':{'type':'set'},extractSymText(db[s]):{'type':'parameter'}} }, e_dict)
      else:
         io_dict[s.lower()] = dict_merge({ 'alias':extractSymText(db[s]), 'headers':{extractSymText(db[s].domains[0]):{'type':'set'},extractSymText(db[s]):{'type':'parameter'}} }, e_dict)
   else:         
      headers = {}
      domdict = {}
      if expandLastCol(db[s], %MAX_VAL_COL%):
         for d in db[s].domains[:-1]:
            symText = extractSymText(d)
            if symText in domdict:
               headers[symText + str(domdict[symText])] = { 'type':'set' }
               domdict[symText] = domdict[symText]+1
            else:
               domdict[symText] = 1
               headers[symText] = { 'type':'set' }
      
         for r in db[s].domains[-1]:
            headers[r.key(0).lower()] = { 'type':'parameter' }
      else:
         for d in db[s].domains:
            symText = extractSymText(d)
            if symText in domdict:
               headers[symText + str(domdict[symText])] = { 'type':'set' }
               domdict[symText] = domdict[symText]+1
            else:
               domdict[symText] = 1
               headers[symText] = { 'type':'set' }
            
         headers[extractSymText(db[s])] = { 'type':'parameter' }
      
      auto = { 'alias':extractSymText(db[s]), 'headers':headers }
      io_dict[s.lower()] = dict_merge(auto,e_dict)

if SOtrueLen>0:
   if SOhidden:
      io_dict['scalars_out'] = { 'alias':'Output Scalars', 'hidden':True, 'symnames':scalar_output_sym, 'symtext':scalar_output_text, 'symtypes':scalar_output_type, 'count':len(scalar_output_sym), 'headers':{'Scalar':{'type':'set'},'Description':{'type':'acronym'},'Value':{'type':'acronym'}} }
   else:
      io_dict['scalars_out'] = { 'alias':'Output Scalars', 'symnames':scalar_output_sym, 'symtext':scalar_output_text, 'symtypes':scalar_output_type, 'count':len(scalar_output_sym), 'headers':{'Scalar':{'type':'set'},'Description':{'type':'acronym'},'Value':{'type':'acronym'}} }
config['gamsOutputFiles'] = io_dict

try:
   if not os.path.exists('./conf/'):
      os.makedirs('./conf/')
except OSError:
   print ('Error: Creating directory: conf')
with open('conf/GMSIO_config.json', 'w') as f:
   json.dump(config, f, indent=4, sort_keys=False)
db.__del__()
import os
from platform import system
import subprocess
for s in rmfiles:
   os.remove(s.lower())
   
def get_r_path():
    try:
        with open(os.path.join(r"%gams.sysdir% ".strip(), 'GMSWebUI', 'conf', 'rpath.conf')) as f:
            RPath = f.readline().strip()
            return RPath
    except:
        pass
    def major_minor_micro(version):
        RverTmp = search('(\d+)\.(\d+)\.(\d+)', version)
        if RverTmp is None:
           major, minor, micro = (0,0,0)
        else:
           major, minor, micro = RverTmp.groups()
        return int(major), int(minor), int(micro)
    def major_minor(version):
        RverTmp = search('(\d+)\.(\d+)', version)
        if RverTmp is None:
           major, minor = (0,0)
        else:
           major, minor = RverTmp.groups()
        return int(major), int(minor)
    if system() == "Windows":
        import winreg
        try:
            aReg = winreg.ConnectRegistry(None,winreg.HKEY_LOCAL_MACHINE)
            aKey = winreg.OpenKey(aReg, r"SOFTWARE\R-Core\R")
        except FileNotFoundError:
            return ""
        paths = []
        for i in range(20):
            try:
                asubkey_name = winreg.EnumKey(aKey,i)
                asubkey = winreg.OpenKey(aKey,asubkey_name)
                paths.append(winreg.QueryValueEx(asubkey, "InstallPath")[0])
            except EnvironmentError:
                break
        if len(paths) == 0:
           return ""
        latestRPath = max(paths, key=major_minor_micro)
        latestR = major_minor_micro(latestRPath)
        latestRPath = latestRPath + os.sep + "bin" + os.sep
    elif system() == "Darwin":
        RPath = r"/Library/Frameworks/R.framework/Versions"
        RVers = os.listdir(RPath)
        latestRPath = max(RVers, key=major_minor)
        latestR = major_minor(latestRPath)
        latestRPath = RPath + os.sep + latestRPath + os.sep + "Resources" + os.sep + "bin" + os.sep
    else:
        RPath = subprocess.run(['which', 'Rscript'], stdout=subprocess.PIPE).stdout
        if not len(RPath):
           latestR = (0, 0)
        else:
           RPath = RPath.decode('utf-8').strip().strip('Rscript')

    if latestR[0] < 3 or latestR[0] == 3 and latestR[1] < 5:
      os.environ["PYEXCEPT"] = "RVERSIONERROR"
      raise FileNotFoundError('Bad R version')
    return latestRPath
RPath = get_r_path()

os.environ["RPATH"] = RPath
if os.path.exists(r"%gams.sysdir%GMSWebUI%system.dirsep%library"):
    sysdir = r"%gams.sysdir% ".strip().replace("\\","/") + "GMSWebUI/library"
else:
    sysdir = ""
with open("runapp.R", "w") as f: 
   f.write("RLibPath <- '"+sysdir+"'\n")
   f.write("""
if(RLibPath == ""){{
   RLibPath <- NULL
}}
if(!'shiny'%in%installed.packages(lib.loc = RLibPath)[, 'Package']){{
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  newPackages <- c("methods", "utils", "grDevices", "httpuv", "mime", 
                   "jsonlite", "xtable", "digest", "htmltools", "R6", 
                   "sourcetools", "later", "promises", "tools", "crayon", "rlang", "shiny")
  for(pkg_name in newPackages){{
    if(!is.null(RLibPath)){{
      pkg_path <- NULL
      try(pkg_path <- list.files(RLibPath, paste0("^", pkg_name, "_.*\\\\.zip$"), 
                                 full.names = TRUE, recursive = TRUE))
      if(length(pkg_path)){{
        install.packages(pkg_path[[1]], lib = RLibPath, repos = NULL, 
                         type="binary", dependencies = FALSE)
        next
      }}
    }}
    install.packages(pkg_name, lib = if(length(RLibPath)) RLibPath else .libPaths()[[1]], repos = 'https://cloud.r-project.org', dependencies = TRUE)
  }}
  options(install.packages.check.source = checkSourceDefault)
  rm(checkSourceDefault)
}}
library("shiny", character.only = TRUE, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE, lib.loc = RLibPath)
shiny::runApp(appDir = file.path("{0}"), launch.browser=TRUE)""".format(r"%WEBUIDIR% ".strip().replace("\\","/")))

if %mkApp%>0:
    fn_model = "%fn%"
    fe_model = "%fe%"
    fp_model = r"%fp% ".strip()
    gams_sysdir = r"%gams.sysdir% ".strip()
    
    if system() == "Windows":
        with open(fn_model + ".bat", "w") as f:
            f.write('''start /min "" cmd /C ""{0}Rscript" --vanilla "{1}runapp.R" -modelPath="{2}" -gamsSysDir="{3}""'''.format(RPath, fp_model, os.path.join(fp_model,fn_model + fe_model), gams_sysdir))
    elif system() == "Darwin":
        from shutil import rmtree
        with open(fn_model + ".applescript", "w") as f:
            f.write('''do shell script "'{0}Rscript' --vanilla '{1}runapp.R' -modelPath='{2}' -gamsSysDir='{3}'"'''.format(RPath, fp_model, os.path.join(fp_model,fn_model + fe_model), gams_sysdir))
        if os.path.isdir(fn_model + ".app"):
           rmtree(fn_model + ".app")
        subprocess.call(["osacompile", "-o", fn_model + ".app", fn_model + ".applescript"])
        os.remove(fn_model + ".applescript")
        appLogoPath = r"%appLogoPath% ".strip()
        if(len(appLogoPath) > 0 and os.path.isfile(appLogoPath)):
          logoPath = appLogoPath
        else:
          logoPath = os.path.join(r"%WEBUIDIR% ".strip(), "resources", "macos", "gmslogo.icns")
        subprocess.call(["cp", logoPath, fn_model + ".app/Contents/Resources/"])

        plistPath = fn_model + ".app/Contents/Info.plist"
        with open(plistPath, "r") as f:
            plist = f.readlines()

        idx = len(plist) - 2
        
        plist.insert(idx, "  <key>CFBundleIconFile</key>\n  <string>" + os.path.basename(logoPath) +"</string>\n")
        with open(plistPath, "w") as f:
           plist = "".join(plist)
           f.write(plist)
    else:
        pass
$offembeddedCode
$hiddencall rm -rf __pycache__
$ifthen not errorfree
$if %sysenv.PYEXCEPT% == "RVERSIONERROR" $abort "R version 3.5 or higher required. Set the path to the RScript executable manually by placing a file: 'rpath.conf' that contains a single line specifying this path in the '<GAMSroot>/GMSWebUI/conf/' directory."
$terminate
$endif
$ifthen %WEBUI%=="launch"
$  hiddencall cd . && "%sysenv.RPATH%Rscript" "--vanilla" "%fp%runapp.R" -modelPath="%fp%%fn%%fe%" -gamsSysDir="%gams.sysdir%"
$  if errorlevel 1 $abort Problems executing the GAMS WebUI. Make sure you have a valid WebUI installation.
$endif
$terminate
