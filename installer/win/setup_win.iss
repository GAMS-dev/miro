; credit to: The RInno package (https://github.com/ficonsulting/RInno/blob/master/LICENSE) (GPL 3 licensed)

#define WebUIName "GAMS WebUI"
#define RMajor 3
#define RMinor 5
#define RPatch 1
#define WebUIPublisher "GAMS Software GmbH/GAMS Development Corp."
#define WebUIURL "gams.com"
#define docURL "https://www.gams.com/latest/webui/"

[Setup]
AppName = {#WebUIName}
AppId = {{R77BWYZ1-KM2Y-WCD5-E8JC-Z7CIVDSLX2Y4}
DefaultDirName={code:GetDefaultDirName}
DefaultGroupName = GAMS WebUI
OutputBaseFilename = GAMS_WebUI-{#WebUIVersion}
SetupIconFile = setup.ico
AppVersion = {#WebUIVersion}
AppPublisher = {#WebUIPublisher}
AppPublisherURL = gams.com
AppSupportURL = gams.com/support
AppUpdatesURL = {#WebUIURL}
PrivilegesRequired = lowest
InfoBeforeFile = infobefore.txt
InfoAfterFile = infoafter.txt
Compression = lzma2/ultra64
SolidCompression = yes
LicenseFile=../../LICENSE
ArchitecturesInstallIn64BitMode = x64
DisableProgramGroupPage = yes
DirExistsWarning = no
WizardSmallImageFile=gams_small.bmp,gams_small2.bmp
WizardImageFile=gams_left.bmp,gams_left2.bmp

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
[Types]
Name: "full"; Description: "Full installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom
[Components]
Name: "program"; Description: "WebUI Files"; Types: full custom; Flags: fixed
Name: "examples"; Description: "Sample Models"; Types: full
[Files]
Source: "../../R-{#RMajor}.{#RMinor}.{#RPatch}-win.exe"; DestDir: "{tmp}"; Check: installR
Source: "../../LICENSE"; Flags: dontcopy noencryption
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;Components: program
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;Components: program
Source: "../../library/*"; DestDir: "{app}\library\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../conf/*"; DestDir: "{app}\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../JS/*"; DestDir: "{app}\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../modules/*"; DestDir: "{app}\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../R/*"; DestDir: "{app}\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../UI/*"; DestDir: "{app}\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../www/*"; DestDir: "{app}\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../www/*"; DestDir: "{app}\app.R"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../www/*"; DestDir: "{app}\global.R"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../model/loadCSV.gms"; DestDir: "{app}\inclib"; Flags: ignoreversion;Components: program
Source: "../../model/webui.gms"; DestDir: "{app}\inclib"; Flags: ignoreversion;Components: program

Source: "../../model/kport/conf/config.json"; DestDir: "{userdocs}\GMSWebUI\examples\kport\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/kport/kport.gms"; DestDir: "{userdocs}\GMSWebUI\examples\kport"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock/conf/config.json"; DestDir: "{userdocs}\GMSWebUI\examples\pickstock\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock/dowjones2016.csv"; DestDir: "{userdocs}\GMSWebUI\examples\pickstock"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock/pickstock.gms"; DestDir: "{userdocs}\GMSWebUI\examples\pickstock"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock_live/conf/config.json"; DestDir: "{userdocs}\GMSWebUI\examples\pickstock_live\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock_live/pickstock_live.gms"; DestDir: "{userdocs}\GMSWebUI\examples\pickstock_live"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport/conf/config.json"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport/customRenderer/trnsport_custom.R"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport/customRenderer/us-states.geojson"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport/trnsport.gms"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport_live/conf/config.json"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport_live\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport_live/customRenderer/trnsport_custom.R"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport_live\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport_live/customRenderer/us-states.geojson"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport_live\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/trnsport_live/trnsport_live.gms"; DestDir: "{userdocs}\GMSWebUI\examples\trnsport_live"; Flags: ignoreversion;Components: examples
[Icons]
Name: "{group}\{cm:UninstallProgram,{#WebUIName}}"; Filename: "{uninstallexe}"

[Run]
Filename: "{tmp}\R-{#RMajor}.{#RMinor}.{#RPatch}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Check: installR; Flags: skipifdoesntexist; StatusMsg: "Installing R {#RMajor}.{#RMinor}.{#RPatch}"
Filename: "{#docURL}"; Flags: postinstall shellexec runasoriginaluser shellexec skipifsilent; StatusMsg: "Open documentation"

[Code]
function needR(): boolean;
var
    major: Integer;
    minor: Integer;
    patch: Integer;
    installR: boolean;
begin
  installR := true;
  for major := {#RMajor} to 10 do
    begin
    for minor := 1 to 20 do
      begin
      if (major = {#RMajor}) and (minor < {#RMinor}) then Continue;
      for patch := 1 to 10 do
          begin
              if (major = {#RMajor}) and (minor = {#RMinor}) and (patch < {#RPatch}) then Continue;
              if RegKeyExists(HKLM, 'Software\R-Core\R\' + IntToStr(major) + '.' + IntToStr(minor) + '.' + IntToStr(patch)) or RegKeyExists(HKCU, 'Software\R-Core\R\' + IntToStr(major) + '.' + IntToStr(minor) + '.' + IntToStr(patch)) then
              begin
                installR := false;
                break;
              end;
          end;
      end;
    end;
  Result := installR;
end;

function GetDefaultDirName(Param: string): string;
var
  Path : String;
begin
  if RegQueryStringValue(HKCU, 'gams.location', '', Path) then
  begin
    Result := Path;
  end
  else if RegQueryStringValue(HKCR, 'gams.location', '', Path) then
  begin
      Result := Path;
  end
  else
    WizardForm.Close;
end;

function InitializeSetup: Boolean;
begin
  Result := True;
  if not RegKeyExists(HKCU, 'gams.location') and not RegKeyExists(HKCR, 'gams.location') then
  begin
    MsgBox('In order to install the GAMS WebUI, a valid installation of GAMS is required. You can download GAMS from: www.gams.com.', mbError, MB_OK);
    Result := False;
  end;
end;
