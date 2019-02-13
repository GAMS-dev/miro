rem create an inno installation file from a freshly installed GAMS version in c:\gams\win64\innofiles
set GITDIR=C:\Users\Michael\Documents\WC\gmswebui\
set GPRODUCTS=C:\home\distrib\porting\products\src\
set INSTALLER=C:\Users\Michael\Downloads\26.1.0x64.exe
set CERT=C:\Users\Michael\Documents\comodo_cert.p12
IF NOT DEFINED CERTPW SET CERTPW=xxx
IF NOT DEFINED CLOUDPW SET CLOUDPW=xxx
IF NOT DEFINED CLOUDUSER SET CLOUDUSER=mbussieck

rem No changes beyond this point
set GAMSDIR=c:\gams\win64\innofiles\
if exist C:\gams\win64 goto cont0
echo %GAMSDIR% does not exist
goto end
:cont0
if exist %GITDIR% goto cont1
echo %GITDIR% does not exist
goto end
:cont1
if not x%CLOUDPW%==xxxx goto cont2
echo no password for the cloud is set. Set via CLOUDPW and CLOUDUSER
goto end
:cont2
if not x%CERTPW%==xxxx goto cont3
echo no password for the comodo certificate is set. Set via CERTPW
goto end
:cont3

rem goto fast

rem quite install of GAMS
cd C:\gams\win64
%INSTALLER% /sp- /silent /dir=c:\gams\win64\innofiles /MERGETASKS='!desktopicon'
if exist %GAMSDIR% goto cont4
echo %GAMSDIR% does not exist
goto end
:cont4

rem Cleanup installation
cd %GAMSDIR%
rm -f .uninstinfo.ini unins000.dat unins000.exe gamside.url

rem R installation
wget --no-check-certificate --user=%CLOUDUSER% --password=%CLOUDPW% https://cloud.gams.com/remote.php/webdav/GMSR_win.zip
gmsunzip -qq -o GMSR_win.zip
rm -f GMSR_win.zip

rem Python modules
cd GMSPython\Scripts
pip install matplotlib pandas numpy xlsxwriter pandas_datareader geocoder
cd %GAMSDIR%

:fast
rem miro libinclude
cd %GAMSDIR%
copy /Y %GITDIR%model\miro.gms inclib

mkdir miro
rm -rf miro\*
cd miro
cp -r %GITDIR%JS .
cp -r %GITDIR%R .
cp -r %GITDIR%UI .
cp -r %GITDIR%conf .
cp -r %GITDIR%modules .
cp -r %GITDIR%resources .
cp -r %GITDIR%tools .
cp -r %GITDIR%www .
cp    %GITDIR%LICENSE .
cp    %GITDIR%app.R .
cp    %GITDIR%global.R .

rem APILIB Example models
cd %GITDIR%model\kport
gmszip -r kport_conf.zip conf\config.json static customRenderer
cp kport_conf.zip kport.gms %GAMSDIR%datalib_ml
rm -f kport_conf.zip

cd %GITDIR%model\pickstock
gmszip -r pickstock_conf.zip conf\config.json static customRenderer
cp pickstock_conf.zip pickstock.gms dowjones2016.csv %GAMSDIR%datalib_ml
rm -f pickstock_conf.zip

cd %GITDIR%model\pickstock_live
gmszip -r pickstock_live_conf.zip conf\config.json static customRenderer
cp pickstock_live_conf.zip pickstock_live.gms %GAMSDIR%datalib_ml
rm -f pickstock_live_conf.zip

cd %GITDIR%model\transport
gmszip -r transport_conf.zip conf\config.json static customRenderer
cp transport_conf.zip transport.gms %GAMSDIR%datalib_ml
rm -f transport_conf.zip

cd %GITDIR%model\transport_live
gmszip -r transport_live_conf.zip conf\config.json static customRenderer
cp transport_live_conf.zip transport_live.gms %GAMSDIR%datalib_ml
rm -f transport_live_conf.zip

cd %GITDIR%
cp -f mkconfig.inc datalib.glb %GAMSDIR%datalib_ml

cd %GAMSDIR%datalib_ml
echo $include mkconfig.inc >> kport.gms
echo $include mkconfig.inc >> pickstock.gms
echo $include mkconfig.inc >> pickstock_live.gms
echo $include mkconfig.inc >> transport.gms
echo $include mkconfig.inc >> transport_live.gms

rem Newer cmex
cd %GAMSDIR%
copy /Y %GPRODUCTS%gamscmex\optgams.def
copy /Y %GPRODUCTS%gamscmex\gamserrs.txt
copy /Y %GPRODUCTS%..\..\btree\gamscmex\wei\gamscmex.exe

cd %GAMSDIR%..
mkdir output
rm -rf output\*
copy /Y %GPRODUCTS%global\gams.ico 
copy /Y %GPRODUCTS%global\gams_left.bmp
copy /Y %GPRODUCTS%global\gams_small.bmp
copy /Y %GITDIR%miro.iss
Compil32.exe /cc miro.iss
rem when ready sign the installer
rem move output\setup.exe output\miro_windows_x64_64.exe
rem rm miro.iss gams.ico gams_left.bmp gams_small.bmp
rem "c:\Program Files (x86)\Windows Kits\10\bin\10.0.16299.0\x64\signtool" sign -v -f %CERT% -p %CERTPW% -tr http://timestamp.comodoca.com/rfc3161 output/miro_windows_x64_64.exe
:end
