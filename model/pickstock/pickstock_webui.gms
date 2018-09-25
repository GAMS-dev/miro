$onExternalInput
$kill stockData symbol date
*$killUel
$offExternalInput
$offdigit
$batInclude loadCSV scalars
$batInclude loadCSV stockdata date symbol
$onmulti
$if setenv PICKSTOCK_MAXSTOCK Scalar maxstock / %sysEnv.PICKSTOCK_MAXSTOCK% /
$if setenv PICKSTOCK_TRAININGDAYS Scalar trainingdays / %sysEnv.PICKSTOCK_TRAININGDAYS% /
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
webuis1_(webui0_) = sum(webui1_, partOfPortfolio(webui0_,webui1_));
partOfPortfolio(webuis1_,wHdr) = partOfPortfolio(webuis1_,wHdr) + eps;
option clear=webuis1_;
webuis1_(webui0_) = sum(webui1_, dowVSindex(webui0_,webui1_));
dowVSindex(webuis1_,fHdr) = dowVSindex(webuis1_,fHdr) + eps;
option clear=webuis1_;
webuis1_(webui0_) = sum(webui1_, abserror(webui0_,webui1_));
abserror(webuis1_,errHdr) = abserror(webuis1_,errHdr) + eps;
option clear=webuis1_;
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
writeCSVParam(db["partOfPortfolio"],"gmswebui.gdx")
writeCSVParam(db["dowVSindex"],"gmswebui.gdx")
writeCSVParam(db["abserror"],"gmswebui.gdx")
with open('scalars_out.csv', 'w') as f:
   f.write('Scalar,Description,Value\n')
   if(len(db['lastDayTraining'])):
      f.write('lastDayTraining,"last date of training period ### vertical marker in chart",' + str(db['lastDayTraining'].first_record().key(0)) + '\n')
   f.closed
db.__del__()
endEmbeddedCode
execute 'rm -f gmswebui.gdx';
