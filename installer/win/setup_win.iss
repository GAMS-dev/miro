#define MyAppName "GAMS WebUI"
#define RMajor 3
#define RMinor 5
#define RPatch 1
#define MyAppPublisher "GAMS Software GmbH/GAMS Development Corp."
#define MyAppURL "gams.com"

[Setup]
AppName = GAMS WebUI
AppId = {{R77BWYZ1-KM2Y-WCD5-E8JC-Z7CIVDSLX2Y4}
DefaultDirName={code:GetDefaultDirName}
DefaultGroupName = GAMS WebUI
OutputBaseFilename = GAMS_WebUI-{#MyAppVersion}
SetupIconFile = setup.ico
AppVersion = {#MyAppVersion}
AppPublisher = {#MyAppPublisher}
AppPublisherURL = gams.com
AppSupportURL = gams.com/support
AppUpdatesURL = {#MyAppURL}
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
Source: "../../LICENSE"; Flags: dontcopy noencryption
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;Components: program
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;Components: program
; Source: "../../library/*"; DestDir: "{app}\library\*"; Flags: ignoreversion recursesubdirs;Components: program
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
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

[Run]
Filename: "https://www.gams.com/latest/webui/"; Flags: postinstall shellexec runasoriginaluser shellexec skipifsilent; StatusMsg: "Open documentation"

[Code]
function needR(): boolean;
var
    major: Integer;
    minor: Integer;
    patch: Integer;
    Rneeded: boolean;
begin
  Rneeded := true;
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
                Rneeded := false;
                break;
              end;
          end;
      end;
    end;
  Result := Rneeded;
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
    MsgBox('Please install GAMS first, before installing the WebUI.', mbError, MB_OK);
    Result := False;
  end;
end;
