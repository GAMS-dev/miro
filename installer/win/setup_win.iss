; credit to: The RInno package (https://github.com/ficonsulting/RInno/blob/master/LICENSE) (GPL 3 licensed)

#define MIROName "GAMS MIRO"
#define RMajor 3
#define RMinor 5
#define RPatch 1
#define MIROPublisher "GAMS Software GmbH/GAMS Development Corp."
#define MIROURL "gams.com"
#define docURL "https://www.gams.com/miro/"

[Setup]
AppName = {#MIROName}
AppId = {{R77BWYZ1-KM2Y-WCD5-E8JC-Z7CIVDSLX2Y4}
DefaultDirName={code:GetDefaultDirName}
DefaultGroupName = GAMS MIRO
OutputBaseFilename = GAMS_MIRO-{#MIROVersion}
SetupIconFile = setup.ico
AppVersion = {#MIROVersion}
AppPublisher = {#MIROPublisher}
AppPublisherURL = gams.com
AppSupportURL = gams.com/support
AppUpdatesURL = {#MIROURL}
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
Name: "program"; Description: "MIRO Files"; Types: full custom; Flags: fixed
Name: "examples"; Description: "Sample Models"; Types: full
[Files]
Source: "../../R-{#RMajor}.{#RMinor}.{#RPatch}-win.exe"; DestDir: "{tmp}"; Check: installR
Source: "../../LICENSE"; Flags: dontcopy noencryption
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;Components: program
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;Components: program
Source: "../../app.R"; DestDir: "{app}\miro"; Flags: ignoreversion;Components: program
Source: "../../global.R"; DestDir: "{app}\miro"; Flags: ignoreversion;Components: program
Source: "../../LICENSE"; DestDir: "{app}\miro"; Flags: ignoreversion;Components: program
Source: "../../library/*"; DestDir: "{app}\miro\library\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../conf/*"; DestDir: "{app}\miro\conf\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../JS/*"; DestDir: "{app}\miro\JS\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../modules/*"; DestDir: "{app}\miro\modules\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../R/*"; DestDir: "{app}\miro\R\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../UI/*"; DestDir: "{app}\miro\UI\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../www/*"; DestDir: "{app}\miro\www\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../tools/*"; DestDir: "{app}\miro\tools\*"; Flags: ignoreversion recursesubdirs;Components: program
Source: "../../resources/hcube_submission.gms"; DestDir: "{app}\resources"; Flags: ignoreversion;Components: program
Source: "../../model/miro.gms"; DestDir: "{app}\inclib"; Flags: ignoreversion;Components: program

Source: "../../model/kport/conf/kport.json"; DestDir: "{userdocs}\MIRO_examples\kport\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/kport/kport.gms"; DestDir: "{userdocs}\MIRO_examples\kport"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock/conf/pickstock.json"; DestDir: "{userdocs}\MIRO_examples\pickstock\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock/dowjones2016.csv"; DestDir: "{userdocs}\MIRO_examples\pickstock"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock/pickstock.gms"; DestDir: "{userdocs}\MIRO_examples\pickstock"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock_live/conf/pickstock_live.json"; DestDir: "{userdocs}\MIRO_examples\pickstock_live\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/pickstock_live/pickstock_live.gms"; DestDir: "{userdocs}\MIRO_examples\pickstock_live"; Flags: ignoreversion;Components: examples
Source: "../../model/transport/conf/transport.json"; DestDir: "{userdocs}\MIRO_examples\transport\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/transport/customRenderer/transport_custom.R"; DestDir: "{userdocs}\MIRO_examples\transport\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/transport/customRenderer/us-states.geojson"; DestDir: "{userdocs}\MIRO_examples\transport\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/transport/transport.gms"; DestDir: "{userdocs}\MIRO_examples\transport"; Flags: ignoreversion;Components: examples
Source: "../../model/transport_live/conf/transport_live.json"; DestDir: "{userdocs}\MIRO_examples\transport_live\conf"; Flags: ignoreversion;Components: examples
Source: "../../model/transport_live/customRenderer/transport_custom.R"; DestDir: "{userdocs}\MIRO_examples\transport_live\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/transport_live/customRenderer/us-states.geojson"; DestDir: "{userdocs}\MIRO_examples\transport_live\customRenderer"; Flags: ignoreversion;Components: examples
Source: "../../model/transport_live/transport_live.gms"; DestDir: "{userdocs}\MIRO_examples\transport_live"; Flags: ignoreversion;Components: examples
[Icons]
Name: "{group}\{cm:UninstallProgram,{#MIROName}}"; Filename: "{uninstallexe}"

[Run]
Filename: "{tmp}\R-{#RMajor}.{#RMinor}.{#RPatch}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Check: installR; Flags: skipifdoesntexist; StatusMsg: "Installing R {#RMajor}.{#RMinor}.{#RPatch}"
Filename: "{#docURL}"; Flags: postinstall shellexec runasoriginaluser shellexec skipifsilent; StatusMsg: "Open documentation"

[Code]
function installR(): boolean;
var
    major: Integer;
    minor: Integer;
    patch: Integer;
    needR: boolean;
begin
  needR := true;
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
                needR := false;
                break;
              end;
          end;
      end;
    end;
  Result := needR;
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
    MsgBox('In order to install GAMS MIRO, a valid installation of GAMS is required. You can download GAMS from: www.gams.com.', mbError, MB_OK);
    Result := False;
  end;
end;
