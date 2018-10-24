$onmulti
$offmulti
$ondigit
$onuni
alias (*,webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_,webui17_,webui18_,webui19_);
set webuis1_(webui0_);
set webuis2_(webui0_,webui1_);
set webuis3_(webui0_,webui1_,webui2_);
set webuis4_(webui0_,webui1_,webui2_,webui3_);
set webuis5_(webui0_,webui1_,webui2_,webui3_,webui4_);
set webuis6_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_);
set webuis7_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_);
set webuis8_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_);
set webuis9_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_);
set webuis10_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_);
set webuis11_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_);
set webuis12_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_);
set webuis13_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_);
set webuis14_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_);
set webuis15_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_);
set webuis16_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_);
set webuis17_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_);
set webuis18_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_,webui17_);
set webuis19_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_,webui17_,webui18_);
webuis2_(webui0_,webui1_) = sum(webui2_, businfo_Report(webui0_,webui1_,webui2_));
businfo_Report(webuis2_,bus_s) = businfo_Report(webuis2_,bus_s) + eps;
option clear=webuis2_;
webuis2_(webui0_,webui1_) = sum(webui2_, geninfo_Report(webui0_,webui1_,webui2_));
geninfo_Report(webuis2_,gen_s) = geninfo_Report(webuis2_,gen_s) + eps;
option clear=webuis2_;
webuis4_(webui0_,webui1_,webui2_,webui3_) = sum(webui4_, branchinfo_Report(webui0_,webui1_,webui2_,webui3_,webui4_));
branchinfo_Report(webuis4_,branch_s) = branchinfo_Report(webuis4_,branch_s) + eps;
option clear=webuis4_;
webuis1_(webui0_) = sum(webui1_, fuelinfo_Report(webui0_,webui1_));
fuelinfo_Report(webuis1_,fuel_s) = fuelinfo_Report(webuis1_,fuel_s) + eps;
option clear=webuis1_;
execute_unload "gmswebui.gdx";
embeddedCode Python:
from subprocess import run
from shlex import quote

def extractSymText(sym, leavehash=0):
   text = sym.text
   input_tag = 'UIInput:'
   uii = len(input_tag)
   output_tag = 'UIOutput:'
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

      
def writeCSVParam(sym, gdxname='none', max_val_col = 5):
   expand_last_col = expandLastCol(sym, max_val_col)
   if gdxname=='none':
      with open(sym.name.lower()+'.csv', 'w') as f:
         if sym.dimension==1:
            f.write(getCSVHeader(sym, max_val_col)+'\n')
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

      run('gdxdump ' + quote(gdxname) + ' epsout=0 symb='+quote(sym.name)+' cdim='+quote(cdim)+' format=csv header="' + getCSVHeader(sym, -1) + '" > ' + quote(sym.name.lower())+'.csv', shell=True)

gams.wsWorkingDir = "."
db = gams.ws.add_database_from_gdx("gmswebui.gdx")
writeCSVParam(db["businfo_Report"],"gmswebui.gdx", -1)
writeCSVParam(db["geninfo_Report"],"gmswebui.gdx", -1)
writeCSVParam(db["branchinfo_Report"],"gmswebui.gdx", -1)
writeCSVParam(db["fuelinfo_Report"],"gmswebui.gdx", -1)
with open('scalars_out.csv', 'w') as f:
   f.write('Scalar,Description,Value\n')
   f.write('total_cost_Report,"Final objective value",' + str(db['total_cost_Report'].first_record().value) + '\n')
   f.write('version_Report,"version",' + str(db['version_Report'].first_record().value) + '\n')
   f.write('baseMVA_Report,"baseMVA",' + str(db['baseMVA_Report'].first_record().value) + '\n')
   if(len(db['case_input'])):
      f.write('case_input,"Selected Testcase",' + str(db['case_input'].first_record().key(0)) + '\n')
   if(len(db['obj_input'])):
      f.write('obj_input,"Objective function",' + str(db['obj_input'].first_record().key(0)) + '\n')
   if(len(db['LP_solver'])):
      f.write('LP_solver,"LP-solver",' + str(db['LP_solver'].first_record().key(0)) + '\n')
   if(len(db['QCP_solver'])):
      f.write('QCP_solver,"QCP-solver",' + str(db['QCP_solver'].first_record().key(0)) + '\n')
   if(len(db['NLP_solver'])):
      f.write('NLP_solver,"NLP-solver",' + str(db['NLP_solver'].first_record().key(0)) + '\n')
   if(len(db['CNS_solver'])):
      f.write('CNS_solver,"CNS-solver",' + str(db['CNS_solver'].first_record().key(0)) + '\n')
   if(len(db['timeperiod_input'])):
      f.write('timeperiod_input,"Selected time period to solve",' + str(db['timeperiod_input'].first_record().key(0)) + '\n')
   if(len(db['allon_input'])):
      f.write('allon_input,"Turned on gens and/or lines during solve",' + str(db['allon_input'].first_record().key(0)) + '\n')
   if(len(db['linelimits_input'])):
      f.write('linelimits_input,"Type of line limit data to use",' + str(db['linelimits_input'].first_record().key(0)) + '\n')
   if(len(db['genPmin_input'])):
      f.write('genPmin_input,"Data for Generator lower limit",' + str(db['genPmin_input'].first_record().key(0)) + '\n')
   if(len(db['lineloss_input'])):
      f.write('lineloss_input,"Whether to approximate lineloss",' + str(db['lineloss_input'].first_record().key(0)) + '\n')
   if(len(db['qlim_input'])):
      f.write('qlim_input,"Whether to enforce reactive power limits as D-curve circle constraints",' + str(db['qlim_input'].first_record().key(0)) + '\n')
   if(len(db['slim_input'])):
      f.write('slim_input,"Whether to use apparent power limits on line",' + str(db['slim_input'].first_record().key(0)) + '\n')
   if(len(db['ic_input'])):
      f.write('ic_input,"Choosen method for generating initial conditions, i.e. NLP starting point",' + str(db['ic_input'].first_record().key(0)) + '\n')
   if(len(db['iter_input'])):
      f.write('iter_input,"Number of iterations",' + str(db['iter_input'].first_record().key(0)) + '\n')
   if(len(db['times_input'])):
      f.write('times_input,"Time range",' + str(db['times_input'].first_record().key(0)) + '\n')
   if(len(db['ramprates_input'])):
      f.write('ramprates_input,"Type of ramprate data to use",' + str(db['ramprates_input'].first_record().key(0)) + '\n')
   if(len(db['relax_input'])):
      f.write('relax_input,"Whether to relax integer models",' + str(db['relax_input'].first_record().key(0)) + '\n')
   if(len(db['demandbids_input'])):
      f.write('demandbids_input,"Whether to turn on elastic demand bidding",' + str(db['demandbids_input'].first_record().key(0)) + '\n')
   if(len(db['wind_input'])):
      f.write('wind_input,"Whether to turn off wind turbines",' + str(db['wind_input'].first_record().key(0)) + '\n')
   if(len(db['savesol_input'])):
      f.write('savesol_input,"Whether to save the solution as GDX",' + str(db['savesol_input'].first_record().key(0)) + '\n')
   if(len(db['verbose_input'])):
      f.write('verbose_input,"Whether to print input in listing output",' + str(db['verbose_input'].first_record().key(0)) + '\n')
   f.closed
db.__del__()
endEmbeddedCode
execute 'rm -f gmswebui.gdx';
