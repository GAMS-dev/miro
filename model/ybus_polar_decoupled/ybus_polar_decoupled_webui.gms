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
webuis3_(webui0_,webui1_,webui2_) = sum(webui3_, businfo_Report(webui0_,webui1_,webui2_,webui3_));
businfo_Report(webuis3_,businfoHdr) = businfo_Report(webuis3_,businfoHdr) + eps;
option clear=webuis3_;
webuis3_(webui0_,webui1_,webui2_) = sum(webui3_, geninfo_Report(webui0_,webui1_,webui2_,webui3_));
geninfo_Report(webuis3_,geninfoHdr) = geninfo_Report(webuis3_,geninfoHdr) + eps;
option clear=webuis3_;
webuis5_(webui0_,webui1_,webui2_,webui3_,webui4_) = sum(webui5_, branchinfo_Report(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_));
branchinfo_Report(webuis5_,branchinfoHdr) = branchinfo_Report(webuis5_,branchinfoHdr) + eps;
option clear=webuis5_;
webuis2_(webui0_,webui1_) = sum(webui2_, fuelinfo_Report(webui0_,webui1_,webui2_));
fuelinfo_Report(webuis2_,fuelinfoHdr) = fuelinfo_Report(webuis2_,fuelinfoHdr) + eps;
option clear=webuis2_;
execute_unload "gmswebui.gdx";
embeddedCode Python:
def extractSymText(text, leavehash=0):
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

gams.wsWorkingDir = "."
db = gams.ws.add_database_from_gdx("gmswebui.gdx")
writeCSVParam(db["businfo_Report"],"gmswebui.gdx")
writeCSVParam(db["geninfo_Report"],"gmswebui.gdx")
writeCSVParam(db["branchinfo_Report"],"gmswebui.gdx")
writeCSVParam(db["fuelinfo_Report"],"gmswebui.gdx")
with open('scalars_out.csv', 'w') as f:
   f.write('Scalar,Description,Value\n')
   f.write('total_cost_Report,"Final objective value",' + str(db['total_cost_Report'].first_record().value) + '\n')
   f.write('version_Report,"version",' + str(db['version_Report'].first_record().value) + '\n')
   f.write('baseMVA_Report,"baseMVA",' + str(db['baseMVA_Report'].first_record().value) + '\n')
   f.write('QCP_solver,"QCP-solver",' + str(db['QCP_solver'].first_record().key(0)) + '\n')
   f.write('NLP_solver,"NLP-solver",' + str(db['NLP_solver'].first_record().key(0)) + '\n')
   f.write('obj_input,"Objective function",' + str(db['obj_input'].first_record().key(0)) + '\n')
   f.write('timeperiod_input,"Selected time period to solve",' + str(db['timeperiod_input'].first_record().key(0)) + '\n')
   f.write('allon_input,"Turned on gens and/or lines during solve",' + str(db['allon_input'].first_record().key(0)) + '\n')
   f.write('linelimits_input,"Type of line limit data to use",' + str(db['linelimits_input'].first_record().key(0)) + '\n')
   f.write('genPmin_input,"Data for Generator lower limit",' + str(db['genPmin_input'].first_record().key(0)) + '\n')
   f.write('qlim_input,"Whether to enforce reactive power limits as D-curve circle constraints",' + str(db['qlim_input'].first_record().key(0)) + '\n')
   f.write('iter_input,"Number of iterations",' + str(db['iter_input'].first_record().key(0)) + '\n')
   f.write('demandbids_input,"Whether to turn on elastic demand bidding",' + str(db['demandbids_input'].first_record().key(0)) + '\n')
   f.write('savesol_input,"Whether to save the solution as GDX",' + str(db['savesol_input'].first_record().key(0)) + '\n')
   f.write('verbose_input,"Whether to print input in listing output",' + str(db['verbose_input'].first_record().key(0)) + '\n')
   f.closed
db.__del__()
endEmbeddedCode
execute 'rm -f gmswebui.gdx';
