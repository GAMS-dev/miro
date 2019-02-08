* Driver miro.gms to interact with GAMS/MIRO
$offListing
$onText
Put the input and output symbols in $on/offExternalInput and $on/offExternalOutput blocks.

Start your model with miro=launch to start the GAMS/MIRO web app
$offText
$ifi x%gams.MIRO%==xLAUNCH $goto MIRO_L1
$ifi x%gams.MIRO%==xBUILD  $goto MIRO_L1

* MIRO=run is usually set when this model is started from the GAMS/MIRO web app
* The model specific driver modelname_miro.gms will read the input symbols from CSV files
* and after execution write the content of the output symbols as CSV files.
$setNames "%gams.input%" fp fn fe
$include "%fp%%fn%_miro.gms"
$exit

$label MIRO_L1

* Export GDX file with all symbols and current data
$setNames "%gams.input%" fp fn fe
$gdxout "%fn%_cmiro_outdb.gdx"
$unload
$gdxout

$onEmbeddedCode Python:
import json
import subprocess
import os
from copy import deepcopy

input_tag = 'UIInput:'
uii = len(input_tag)
output_tag = 'UIOutput:'
uio = len(output_tag)
pivot_marker = '[MIRO:table]'
gams.wsWorkingDir = '.'
gdxfn = r'%fn%_cmiro_outdb.gdx'   
db = gams.ws.add_database_from_gdx(gdxfn)
i_sym = []
o_sym = []
have_i_scalar = False
have_o_scalar = False
have_o_vescalar = False
pivot_gdx = False

def fillLastDim(symp, symh):
  rc = False
  if symp.number_records>0:
    r = symp.first_record()
    for h in symh:
      try:
        symp.find_record(r.keys[:-1]+h.keys)
      except:
        symp.add_record(r.keys[:-1]+h.keys).value = 4.94066E-324 # EPS
        rc = True
  return rc

def aliasSym(sym):
   text = sym.text
   aliasIdentifier = 'Aliased with '
   if text.startswith(aliasIdentifier):
      return text[len(aliasIdentifier):]
   else:
      return sym.name

def extractSymText(sym,level):
   text = sym.text
   aliasIdentifier = 'Aliased with '
   if text.startswith(aliasIdentifier):
      aliasedSet = text[len(aliasIdentifier):]
      try:
         text = db[aliasedSet].text
      except:
         pass
   if text.startswith(input_tag):
      text = text[uii:]
   elif text.startswith(output_tag):
      text = text[uio:]
   if(len(text) == 0):
      text = sym.name
   if level>0:
      text = text.replace(pivot_marker, '')
   return text.strip()

# Iterate through all symbols
for sym in db:
   if sym.text.startswith(input_tag):
      if type(sym)==GamsParameter:
         if sym.dimension==0:
            have_i_scalar = True
            i_sym.append((sym,'ps'))
         elif pivot_marker in sym.text:
            i_sym.append((sym,'pp'))
            dom = sym.domains_as_strings[-1]
            if not dom=='*': # we don't do any filling with header *
               pivot_gdx = pivot_gdx or fillLastDim(sym,db[aliasSym(db[dom])]) # have all header symbols available in csv
         else:
            i_sym.append((sym,'pr'))
      elif type(sym)==GamsSet:
         rc, subtype, dval, sval = gmdSymbolInfo(sym._database._gmd, sym._sym_ptr, GMD_USERINFO)
         if (sym.dimension==1) and (subtype==1): # singleton set
            have_i_scalar = True
            i_sym.append((sym,'ss'))
         else:
            i_sym.append((sym,'sr'))
      else:
         raise Exception('Unexpected type of external input symbol ' + sym.name)
   if sym.text.startswith(output_tag):
      if type(sym)==GamsParameter:
         if sym.dimension==0:
            have_o_scalar = True
            o_sym.append((sym,'ps'))
         elif pivot_marker in sym.text:
            o_sym.append((sym,'pp'))
         else:
            o_sym.append((sym,'pr'))
      elif type(sym)==GamsSet:
         rc, subtype, dval, sval = gmdSymbolInfo(sym._database._gmd, sym._sym_ptr, GMD_USERINFO)
         if (sym.dimension==1) and (subtype==1): # singleton set
            have_o_scalar = True
            o_sym.append((sym,'ss'))
         else:
            o_sym.append((sym,'sr'))
      elif type(sym)==GamsVariable:
         if sym.dimension==0:
            have_o_vescalar = True
            o_sym.append((sym,'vs'))
         else:
            o_sym.append((sym,'vr'))
      elif type(sym)==GamsEquation:
         if sym.dimension==0:
            have_o_vescalar = True
            o_sym.append((sym,'es'))
         else:
            o_sym.append((sym,'er'))
      else:
         raise Exception('Unexpected type of external input symbol ' + sym.name)

savegdxfn = ''
if pivot_gdx:
   savegdxfn = gdxfn
   gdxfn = r'%fn%_ccmiro_outdb.gdx'   
   db.export(gdxfn)

def keyname(dname,domdict):
   if dname in domdict:
      key = dname + '_' + str(domdict[dname])
      domdict[dname] = domdict[dname]+1
   else:
      key = dname
      domdict[dname] = 1
   return key.lower(), domdict

def headerList(s):
   headers = []
   domdict = {}
   if s[1]=="pp":
      dlist = s[0].domains_as_strings[:-1]
   else:
      dlist = s[0].domains_as_strings
   for d in dlist:
      if d=='*':
         dname = 'uni'
      else:
         dname = d.lower()
      key, domdict = keyname(dname,domdict)
      if d=='*':
         headers.append((key,'set','Universal set'))
      else:
         headers.append((key,'set',extractSymText(db[d],2)))

   if s[1][0]=="s":
      key, domdict = keyname('text',domdict)
      headers.append((key,'set','Set text ' + extractSymText(s[0],2)))
   
   if s[1]=="pr":
      key, domdict = keyname('value',domdict)
      headers.append((key,'parameter',extractSymText(s[0],2)))

   if s[1]=="vr" or s[1]=="er":
      key, domdict = keyname('level',domdict)
      headers.append((key,'parameter','Level')) 
      key, domdict = keyname('marginal',domdict)
      headers.append((key,'parameter','Marginal')) 
      key, domdict = keyname('lower',domdict)
      headers.append((key,'parameter','Lower'))
      key, domdict = keyname('upper',domdict)
      headers.append((key,'parameter','Upper'))
      key, domdict = keyname('scale',domdict)
      headers.append((key,'parameter','Scale'))
      
   if s[1]=="pp":
      for r in s[0].domains[-1]:
         key, domdict = keyname(r.key(0),domdict)
         if r.text=='':
            headers.append((key,'parameter', r.key(0)))
         else:
            headers.append((key,'parameter',r.text))
   return headers

# Now write model specific MIRO driver
with open(r'%fp%%fn%_miro.gms', 'w') as f:
   f.write('$ifi %' + 'gams.MIRO%==run $goto MIRO_RUN\n')
   f.write('\n')
   f.write('* Create csv files of existing data for Excel file creation\n')
   for s in i_sym:
      if (s[1]=='ps') or (s[1]=='ss'): # skip scalars
         continue
      sym = s[0]
      stype = s[1]
      symHeader = headerList(s)
      htext = symHeader[0][0]
      for h in symHeader[1:]:
         htext = htext + ',' + h[0]
      f.write('$ife card(' + sym.name + ')=0 $abort Need data for ' + sym.name + '\n')
      if stype=='pp': # pivot
         f.write('$hiddencall gdxdump ' + gdxfn + ' epsout=0 symb=' + sym.name + ' csvsettext csvallfields cdim=1 header="' + htext + '" format=csv > ' + sym.name.lower() + '.csv\n')
      else:
         f.write('$hiddencall gdxdump ' + gdxfn + ' epsout=0 symb=' + sym.name + ' csvsettext csvallfields header="' + htext + '"  format=csv > ' + sym.name.lower() + '.csv\n')
      f.write('$if errorlevel 1 $abort Problems creating ' + sym.name.lower() + '.csv\n')
   f.write('\n')
   f.write('* Create scalars.csv (if necessary) and produce xlsx file\n')
   f.write('$onembeddedCode Python:\n')
   if have_i_scalar:
      f.write('def getval(ssym,default):\n')
      f.write('   try:\n')
      f.write('     if isinstance(default, str):\n')
      f.write('        return gams.db[ssym].first_record().key(0)\n')
      f.write('     else:\n')
      f.write('        return str(gams.db[ssym].first_record().value)\n')
      f.write('   except:\n')
      f.write('     return str(default)\n')
      f.write('\n')    
      f.write('with open("scalars.csv", "w") as f:\n')
      f.write('   f.write("scalar,description,value\\n")\n')
      for s in i_sym:
         if s[1]=='ss':
            f.write('   f.write("' + s[0].name.lower() + '"+","+"' + extractSymText(s[0],2) + '"+","+getval("' + s[0].name + '","") + "\\n")\n')
         elif s[1]=='ps':
            f.write('   f.write("' + s[0].name.lower() + '"+","+"' + extractSymText(s[0],2) + '"+","+getval("' + s[0].name + '",0) + "\\n")\n')
            f.write('   f.closed\n')
   f.write('fn = r"' + r'%fp%%fn%' + '.xlsx"\n')
   f.write('icsvlist = ["none"')
   if have_i_scalar:
      f.write(',"scalars"')
   for s in i_sym:
      if not (s[1]=='ss' or s[1]=='ps'):
         f.write(',"'+s[0].name.lower()+'"')
   f.write(']\n')
   f.write('\n')
   f.write('# Create example XLSX file\n')
   f.write('def is_number(s):\n')
   f.write('  try:\n')
   f.write('    float(s)\n')
   f.write('    return True\n')
   f.write('  except ValueError:\n')
   f.write('    return False\n')
   f.write('\n')
   f.write('from xlsxwriter.workbook import Workbook\n')
   f.write('import csv\n')
   f.write('\n')       
   f.write('workbook = Workbook(fn)\n')
   f.write('\n')
   f.write('for s in icsvlist[1:]:\n')
   f.write('  worksheet = workbook.add_worksheet(s) #worksheet with symbol name\n')
   f.write('  with open(s + ".csv", "r") as f:\n')
   f.write('    reader = csv.reader(f)\n')
   f.write('    for r, row in enumerate(reader):\n')
   f.write('      for c, col in enumerate(row):\n')
   f.write('        if is_number(col):\n')
   f.write('          worksheet.write(r, c, float(col)) #write the csv file content into it\n')
   f.write('        else:\n')
   f.write('          worksheet.write(r, c, col)\n')
   f.write('workbook.close()\n')
   f.write('$offembeddedCode\n')
   f.write('$if not set MIRO_DEBUG $hiddencall rm -f ' + gdxfn + ' ' + savegdxfn)
   if have_i_scalar:
      f.write(' scalars.csv')
   for s in i_sym:
      if not (s[1]=='ss' or s[1]=='ps'):
         f.write(' '+s[0].name.lower()+'.csv')
   f.write('\n')
   f.write('$exit\n')

   # Now the run part
   f.write('\n')
   f.write('$label MIRO_RUN\n')
   f.write('$ifthen set HCUBE\n')
   f.write('$ set csvHome ..%' + 'system.dirsep%..%' + 'system.dirsep%static%' + 'system.dirsep% ' + '\n')
   f.write('$else\n')
   f.write('$ set csvHome .%' + 'system.dirsep% ' + '\n')
   f.write('$endif\n')
   f.write('$onmultiR\n')
   rm_list = []
   for s in i_sym:
      if not (s[1]=='ss' or s[1]=='ps'):
         symname = s[0].name.lower()
         if s[1][0] == 's':
            extra = 'index=1..' + str(s[0].dimension) + ' text=' + str(s[0].dimension+1)
         elif s[1] == 'pr':
            extra = 'index=1..' + str(s[0].dimension) + ' value=' + str(s[0].dimension+1)
         elif s[1] == 'pp':
            extra = 'index=1..' + str(s[0].dimension-1) + ' valuedim=1 values=' + str(s[0].dimension) + '..lastCol'
         f.write('$hiddencall csv2gdx %csvHome%' + symname + '.csv id=' + symname + ' useheader=y ' + extra + ' > csv2gdx.log\n')
         f.write('$if errorlevel 1 $abort "Problem converting ' + symname + '.csv to GDX. Check csv2gdx.log"\n')
         f.write('$hiddencall rm -f csv2gdx.log\n')
         f.write('$gdxin ' + symname + '\n')
         f.write('$loadDCR ' + symname +'\n')
         f.write('$gdxin\n')
         rm_list.append(symname)
   f.write('$offmulti\n')
   f.write('$if not set MIRO_DEBUG $hiddencall rm -f ')
   for gdxfn in rm_list:
      f.write(gdxfn+'.gdx ')
   f.write('\n\n')
   f.write('$ifthen exist %csvHome%scalars.csv\n')
   f.write('$onembeddedCode Python:\n')
   f.write('import csv\n')
   f.write('import os\n')
   f.write('if os.path.isfile(r"%csvHome%scalars.csv"):\n')
   f.write('   with open(r"%csvHome%scalars.csv", "r") as csvfile:\n')
   f.write('     scalars = csv.reader(csvfile)\n')
   f.write('     next(scalars) # skip header row\n')
   f.write('     for row in scalars:\n')
   f.write('       os.environ["%fn%".upper() + "_" + row[0].upper()] = row[2]\n')
   f.write('$offembeddedCode\n')
   f.write('$onmulti\n')
   for s in i_sym:
      if not (s[1]=='ss' or s[1]=='ps'):
         continue
      if s[1]=='ss':
         stype = 'singleton set'
      elif s[1]=='ps':
         stype = 'scalar'
      sym = s[0]
      f.write('$if setenv ' + '%fn%'.upper() + '_' + sym.name.upper() + ' ' + stype + ' ' + sym.name + ' / %sysEnv.' + '%fn%'.upper() + '_' + sym.name.upper() + '% /\n')
   f.write('$offmulti\n')
   f.write('$endif\n')
   f.write('\n')
   f.write('$ifthen set HCUBE\n')
   f.write('$onmulti\n')
   for s in i_sym:
      if not (s[1]=='ss' or s[1]=='ps'):
         continue
      if s[1]=='ss':
         stype = 'singleton set'
      elif s[1]=='ps':
         stype = 'scalar'
      sym = s[0]
      f.write('$if set HCUBE_SCALAR_' + sym.name.upper() + ' ' + stype + ' ' + sym.name + ' / %HCUBE_SCALAR_' + sym.name.upper() + '% /\n')
   f.write('$offmulti\n')
   f.write('$endif\n')
   f.write('*** Runtime after model ran successfully\n')
   f.write('\n')
   f.write('embeddedCode Python:\n')
   f.write('def fillLastDim(symp, symh):\n')
   f.write('  if symp.number_records>0:\n')
   f.write('    r = symp.first_record()\n')
   f.write('    r.value = r.value # we need to write to the symbol in any case\n')
   f.write('    for h in symh:\n')
   f.write('      try:\n')
   f.write('        symp.find_record(r.keys[:-1]+h.keys)\n')
   f.write('      except:\n')
   f.write('        symp.add_record(r.keys[:-1]+h.keys).value = 4.94066E-324 # EPS\n')
   f.write('  else:\n')
   f.write('    gams.set(symp.name,[])\n')
   pivot_sym = ''
   for s in o_sym:
      if s[1]=='pp':
         dom = s[0].domains_as_strings[-1]
         if not dom=='*':
            f.write('fillLastDim(gams.db["' + s[0].name + '"],gams.db["' + aliasSym(db[dom]) + '"])\n' )
            pivot_sym = pivot_sym + ' ' + s[0].name
   f.write('endEmbeddedCode ' + pivot_sym + '\n')
   fn = '%fn%_eoutdb.gdx'
   f.write('execute_unload "' + fn + '";\n')
   f.write('\n')
   for s in o_sym:
      if s[1]=='ss' or s[1]=='ps' or s[1]=='vs' or s[1]=='es':
         continue
      if s[1]=='pp':
         extra = ' cdim=1'
      else:
         extra = ''
      symname = s[0].name
      symHeader = headerList(s)
      htext = symHeader[0][0]
      
      for h in symHeader[1:]:
         htext = htext + ',' + h[0]
      f.write('$if DEFINED ' + symname + ' execute$card(' + symname + ') ')
      f.write('\'gdxdump ' + fn + ' epsout=0 noheader symb=' + symname + extra + ' header="' + htext + '" format=csv csvsettext csvallfields > ' + symname + '.csv\';')
      f.write(' abort$errorlevel "problems writing ' + symname + '.csv";\n')
   f.write('$if not set MIRO_DEBUG execute "rm -rf ' + fn + '";\n')

   f.write('\n')
   if have_o_scalar or have_o_vescalar:
      f.write('embeddedCode Python:\n')
      if have_o_scalar:
         f.write('def getval(ssym,default):\n')
         f.write('   try:\n')
         f.write('     if isinstance(default, str):\n')
         f.write('        return gams.db[ssym].first_record().key(0)\n')
         f.write('     else:\n')
         f.write('        return str(gams.db[ssym].first_record().value)\n')
         f.write('   except:\n')
         f.write('     return str(default)\n')
         f.write('\n')
         f.write('with open("scalars_out.csv", "w") as f:\n')
         f.write('   f.write("scalar,description,value\\n")\n')
         for s in o_sym:
            if s[1]=='ps':
               f.write('   f.write(\'' + s[0].name.lower() +',"' + extractSymText(s[0],1) + '",\' + getval("' + s[0].name + '",0) + \'\\n\')\n')
            elif s[1]=='ss':
               f.write('   f.write(\'' + s[0].name.lower() +',"' + extractSymText(s[0],1) + '",\' + getval("' + s[0].name + '","") + \'\\n\')\n')
         f.write('   f.closed\n')
      f.write('\n')
      if have_o_vescalar:
         f.write('def veValues(ssym):\n')
         f.write('  try:\n')
         f.write('    r = gams.db[ssym].first_record()\n')
         f.write('    return str(r.level) + "," + str(r.marginal) + "," + str(r.lower) + "," + str(r.upper) + "," + str(r.scale)\n')
         f.write('  except:\n')
         f.write('    return "0.0,0.0,-inf,+inf,1.0"\n')
         f.write('\n')
         f.write('with open("scalarsve_out.csv", "w") as f:\n')
         f.write('   f.write("type,scalar,description,level,marginal,lower,upper,scale\\n")\n')
         for s in o_sym:
            if s[1]=='vs':
               f.write('   f.write(\'var,' + s[0].name.lower() +',"' + extractSymText(s[0],1) + '",\' + veValues("' + s[0].name + '") + \'\\n\')\n')
            elif s[1]=='es':
               f.write('   f.write(\'equ,' + s[0].name.lower() +',"' + extractSymText(s[0],1) + '",\' + veValues("' + s[0].name + '") + \'\\n\')\n')
         f.write('   f.closed\n')
      f.write('endEmbeddedCode\n')
   f.closed
   
# Now the JSON config file
config = { "pageTitle" : "%system.title%",
           "MIROSwitch" : "MIRO=RUN",
           "fileExchange" : "csv",
           "csvDelim" : "," }

io_dict = {}           
for s in i_sym:
   if s[1]=="ss" or s[1]=="ps":
      continue
   headers = {}
   symHeader = headerList(s)
   for h in symHeader:
      headers[h[0]] = { 'type':h[1], 'alias':h[2] }
   
   io_dict[s[0].name.lower()] = { 'alias':extractSymText(s[0],2), 'headers':headers }
if have_i_scalar:
   sn = []
   st = []
   sty = []
   headers = {}   
   for s in i_sym:
      if not (s[1]=="ss" or s[1]=="ps"):
         continue
      sn.append(s[0].name.lower())
      st.append(extractSymText(s[0],2))
      if s[1]=="ss":
         sty.append("set")
      else:
         sty.append("parameter")
   headers['scalar'] = { 'type':'set', 'alias':'Scalar Name' }
   headers['description'] = { 'type':'set', 'alias':'Scalar Description' }
   headers['value'] = { 'type':'set', 'alias':'Scalar Value' }
   io_dict['scalars'] = { 'alias':'Input Scalars', 'symnames':sn, 'symtext':st, 'symtypes':sty, 'headers':headers }

config['gamsInputFiles'] = io_dict
   
io_dict = {}           
for s in o_sym:
   if s[1]=="ss" or s[1]=="ps" or s[1]=='vs' or s[1]=='es' :
      continue
   headers = {}
   symHeader = headerList(s)
   for h in symHeader:
      headers[h[0]] = { 'type':h[1], 'alias':h[2] }
   io_dict[s[0].name.lower()] = { 'alias':extractSymText(s[0],2), 'headers':headers }
   
if have_o_scalar:
   sn = []
   st = []
   sty = []
   headers = {}
   for s in o_sym:
      if not (s[1]=="ss" or s[1]=="ps"):
         continue
      sn.append(s[0].name.lower())
      st.append(extractSymText(s[0],1))
      if s[1]=="ss":
         sty.append("set")
      else:
         sty.append("parameter")
   headers['scalar'] = { 'type':'set', 'alias':'Scalar Name' }
   headers['description'] = { 'type':'set', 'alias':'Scalar Description' }
   headers['value'] = { 'type':'set', 'alias':'Scalar Value' }
   io_dict['scalars_out'] = { 'alias':'Output Scalars', 'symnames':sn, 'symtext':st, 'symtypes':sty, 'count':len(sn), 'headers':headers }

if have_o_vescalar:
   headers = {}   
   headers['type'] = { 'type':'set', 'alias':'Type' }
   headers['scalar'] = { 'type':'set', 'alias':'Scalar Name' }
   headers['description'] = { 'type':'set', 'alias':'Scalar Description' }
   headers['level'] = { 'type':'parameter', 'alias':'Level' }
   headers['marginal'] = { 'type':'parameter', 'alias':'Marginal' }
   headers['lower'] = { 'type':'parameter', 'alias':'Lower' }
   headers['upper'] = { 'type':'parameter', 'alias':'Upper' }
   headers['scale'] = { 'type':'parameter', 'alias':'Scale' }
   io_dict['scalarsve_out'] = { 'alias':'Output Variable/Equation Scalars', 'headers':headers }
config['gamsOutputFiles'] = io_dict

confdir = r'%fp%conf'
if not os.path.exists(confdir):
   os.makedirs(confdir)
with open(confdir + '/GMSIO_config.json', 'w') as f:
   json.dump(config, f, indent=4, sort_keys=False)
$offembeddedCode
   
$include "%fp%%fn%_miro.gms"
$ifi x%gams.MIRO%==xBUILD $terminate

$iftheni.mode x%gams.MIROMODE%==x1
$  set MODEARG LAUNCHHCUBE
$elseifi.mode x%gams.MIROMODE%==x2
$  set MODEARG LAUNCHADMIN
$else.mode
$  set MODEARG ''
$endif.mode

$if not set appLogoPath $set appLogoPath ""
$ifthen.mk not set mkApp
$   set mkApp 0
$else.mk
$   set mkApp 1
$endif.mk
$ifthen dExist %gams.sysdir%miro
$  set MIRODIR %gams.sysdir%miro
$else
$  set MIRODIR %fp%..%system.dirsep%..
$endif

$onEmbeddedCode Python:
from platform import system
from re import search
import os

def get_r_path():
    try:
        with open(os.path.join(r"%gams.sysdir% ".strip(), 'miro', 'conf', 'rpath.conf')) as f:
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
        try:
            with open(os.path.join(r"%gams.sysdir% ".strip(), 'GMSR', 'bin', 'Rscript.exe')) as f:
                return os.path.join(r"%gams.sysdir% ".strip(), 'GMSR', r'bin%system.dirsep% '.strip())
        except:
            pass
    
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
if os.path.exists(r"%gams.sysdir%GMSR%system.dirsep%library"):
    sysdir = r"%gams.sysdir% ".strip().replace("\\","/") + "GMSR/library"
else:
    sysdir = ""
with open("runapp.R", "w") as f: 
   f.write("RLibPath <- '"+sysdir+"'\n")
   f.write("""
if(RLibPath == ""){{
   RLibPath <- NULL
}}else{{
   assign(".lib.loc", RLibPath, envir = environment(.libPaths))
}}
if(!'shiny'%in%installed.packages(lib.loc = RLibPath)[, 'Package']){{
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  newPackages <- c("httpuv", "mime", 
                   "jsonlite", "xtable", "digest", "htmltools", "R6", 
                   "sourcetools", "later", "promises", "crayon", "rlang", 
                   "Rcpp", "BH", "magrittr", "shiny")
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){{
    binFileExt <- "_.*\\\\.zip$"
  }}else{{
    binFileExt <- "_.*\\\\.tgz$"
  }}
  for(pkg_name in newPackages){{
    print(paste0("Installing: ", pkg_name))
    if(!is.null(RLibPath)){{
      pkg_path <- NULL
      try(pkg_path <- list.files(RLibPath, paste0("^", pkg_name, binFileExt), 
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
tryCatch({{
   withCallingHandlers({{
       on.exit(q('no'))
       shiny::runApp(appDir = file.path("{0}"), launch.browser=TRUE)
    }}, warning = function(w){{
       if(!startsWith(conditionMessage(w), "Error"))
          return()
       currwd <- getwd()
       on.exit(setwd(currwd))
       setwd("{1}")
       futile.logger::flog.fatal('%s', w)
       logFiles <- list.files('logs', full.names = TRUE)
       zip::zip('.crash.zip', c(logFiles[file.mtime(logFiles) == max(file.mtime(logFiles))], file.path('conf', c('GMSIO_config.json', 'config.json'))), recurse = FALSE, compression_level = 9)
    }})
}}, error = function(e){{
   currwd <- getwd()
   on.exit(setwd(currwd))
   setwd("{1}")
   try(futile.logger::flog.fatal('%s', e), silent = TRUE)
   logFiles <- list.files('logs', full.names = TRUE)
   zip::zip('.crash.zip', c(logFiles[file.mtime(logFiles) == max(file.mtime(logFiles))], file.path('conf', c('GMSIO_config.json', 'config.json'))), recurse = FALSE, compression_level = 9)
   shiny::runApp(appDir = file.path("{0}", "tools", "crash_report"), launch.browser=TRUE)
}})
q("no")""".format(r"%MIRODIR% ".strip().replace("\\","/"), r"%fp% ".strip().replace("\\","/")))

if %mkApp%>0:
    fn_model = "%fn%"
    fe_model = "%fe%"
    fp_model = r"%fp% ".strip()
    gams_sysdir = r"%gams.sysdir% ".strip()
   
    if system() == "Windows":
        with open(fn_model + ".bat", "w") as f:
            f.write('''start /min "" cmd /C ""{0}Rscript" --vanilla "{1}runapp.R" -modelPath="{2}" -gamsSysDir="{3}" %MODEARG%"'''.format(RPath, fp_model, os.path.join(fp_model,fn_model + fe_model), gams_sysdir))
    elif system() == "Darwin":
        from shutil import rmtree
        with open(fn_model + ".applescript", "w") as f:
            f.write('''do shell script "'{0}Rscript' --vanilla '{1}runapp.R' -modelPath='{2}' -gamsSysDir='{3}' %MODEARG%"'''.format(RPath, fp_model, os.path.join(fp_model,fn_model + fe_model), gams_sysdir))
        if os.path.isdir(fn_model + ".app"):
           rmtree(fn_model + ".app")
        subprocess.call(["osacompile", "-o", fn_model + ".app", fn_model + ".applescript"])
        os.remove(fn_model + ".applescript")
        appLogoPath = r"%appLogoPath% ".strip()
        if(len(appLogoPath) > 0 and os.path.isfile(appLogoPath)):
          logoPath = appLogoPath
        else:
          useDefaultIcon = False
          defaultIconPath = os.path.join(r"%MIRODIR% ".strip(), "resources", "macos", "gmslogo.icns")
          try:
            from PIL import Image, ImageDraw, ImageFont
          except:
            try:
              install("Pillow")
              from PIL import Image, ImageDraw, ImageFont
            except:
              useDefaultIcon = True
          if useDefaultIcon:
            logoPath = defaultIconPath
          else:
            try:
              img = Image.open(os.path.join(r"%MIRODIR% ".strip(), "resources", "macos", "default.ico"))
              img = img.convert("RGB")
              draw = ImageDraw.Draw(img)
              draw.rectangle([0,144,255,192],fill=(34,45,50),outline=(243,150,25))
              fnt = ImageFont.truetype('/Library/Fonts/Verdana.ttf', 30)
              txtX = 128-len(fn_model)*17/2
              draw.text((txtX,150), fn_model, font=fnt, fill=(243,150,25))
              img.save('logo.icns')
              logoPath = os.path.join(fp_model, 'logo.icns')
            except:
              logoPath = defaultIconPath
        subprocess.call(["cp", "-f", logoPath, fn_model + ".app/Contents/Resources/applet.icns"])
        try:
          os.remove('logo.icns')
        except:
          pass
        
    else:
        pass
$offembeddedCode
$hiddencall rm -rf __pycache__

$ifthen not errorfree
$ if %sysenv.PYEXCEPT% == "RVERSIONERROR" $abort "R version 3.5 or higher required. Set the path to the RScript executable manually by placing a file: 'rpath.conf' that contains a single line specifying this path in the '<GAMSroot>/miro/conf/' directory."
$ terminate
$endif
$hiddencall cd . && "%sysenv.RPATH%Rscript" "--vanilla" "%fp%runapp.R" -modelPath="%fp%%fn%%fe%" -gamsSysDir="%gams.sysdir%" %MODEARG%
$if errorlevel 1 $abort Problems executing MIRO as web app. Make sure you have a valid MIRO installation.
$terminate
