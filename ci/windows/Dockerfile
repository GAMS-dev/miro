FROM mcr.microsoft.com/windows/server:ltsc2022

SHELL ["powershell", "-executionpolicy", "bypass", "-command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue'; $verbosePreference='Continue'; Set-PSDebug -Trace 0;"]

ARG NODE_VERSION=24.16.0
ENV NODE_VERSION=${NODE_VERSION}

ARG GPG_VERSION=5.0.2
ENV GPG_VERSION=${GPG_VERSION}

ARG YARN_VERSION=1.22.22
ENV YARN_VERSION=${YARN_VERSION}

ARG R_VERSION=4.4.3
ENV R_VERSION=${R_VERSION}

ARG INNOEXTRACT_VERSION=1.9
ENV INNOEXTRACT_VERSION=${INNOEXTRACT_VERSION}

ARG GAMS_VERSION=49.3.0
ENV GAMS_VERSION=${GAMS_VERSION}

RUN Invoke-WebRequest $('https://files.gpg4win.org/gpg4win-{0}.exe' -f $env:GPG_VERSION) -OutFile 'gpg4win.exe' -UseBasicParsing ; \
    Start-Process .\gpg4win.exe -ArgumentList '/S' -NoNewWindow -Wait ;

RUN iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1')) ; \
    choco install -y git python

RUN choco install -y nodejs --version="$env:NODE_VERSION"

RUN [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 ; \
    Invoke-WebRequest $('https://classic.yarnpkg.com/downloads/{0}/yarn-{0}.msi' -f $env:YARN_VERSION) -OutFile yarn.msi -UseBasicParsing ; \
    Start-Process msiexec.exe -ArgumentList '/i', 'yarn.msi', '/quiet', '/norestart' -NoNewWindow -Wait

RUN New-Item $($env:APPDATA + '\npm') -Type Directory ; \
    $env:PATH = 'C:\nodejs;{0}\npm;C:\yarn\bin;{1}' -f $env:APPDATA, $env:PATH ; \
    Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\' -Name Path -Value $env:PATH

RUN Invoke-WebRequest "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe" -OutFile "Rtools.exe" -UseBasicParsing ; \
    Start-Process Rtools.exe -ArgumentList '/TYPE=full', '/VERYSILENT', -NoNewWindow -Wait

RUN Invoke-WebRequest $('https://constexpr.org/innoextract/files/innoextract-{0}-windows.zip' -f $env:INNOEXTRACT_VERSION) -OutFile innoextract.zip -UseBasicParsing ; \
    Expand-Archive -Path innoextract.zip -DestinationPath c:\innoextract ; \
    $env:PATH = 'C:\innoextract;{0}' -f $env:PATH ; \
    Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\' -Name Path -Value $env:PATH

RUN Invoke-WebRequest "https://aka.ms/vs/16/release/vs_community.exe" -OutFile "$env:TEMP\vs_community.exe" -UseBasicParsing ; \
    & "$env:TEMP\vs_community.exe" --add Microsoft.Component.MSBuild Microsoft.VisualStudio.Component.VC.Tools.x86.x64 Microsoft.VisualStudio.Component.Windows10SDK Microsoft.VisualStudio.Component.VC.Redist.14.Latest Microsoft.VisualStudio.Component.VC.CoreIde Microsoft.VisualStudio.Component.Windows10SDK.19041 Microsoft.VisualStudio.ComponentGroup.NativeDesktop.Core Microsoft.VisualStudio.Component.VC.ATL Microsoft.Component.VC.Runtime.UCRTSDK Microsoft.VisualStudio.Component.VC.ATLMFC Microsoft.VisualStudio.Component.VC.140 Microsoft.VisualStudio.Component.VC.v141.x86.x64 --quiet --wait --norestart --noUpdateInstaller | Out-Default ; \
    $env:Path = [System.Environment]::GetEnvironmentVariable('Path','Machine') + ';' + [System.Environment]::GetEnvironmentVariable('Path','User')

RUN Invoke-WebRequest $('https://d37drm4t2jghv5.cloudfront.net/distributions/{0}/windows/windows_x64_64.exe' -f $env:GAMS_VERSION) -OutFile "gmsinstall.exe" -UseBasicParsing ; \
    ./gmsinstall.exe /sp- /noLicense=yes /silent /noIcons /desktopIcons=no /currentuser /dir=C:/gams ; \
    Start-Sleep -Seconds 60 ; \
    Remove-Item gmsinstall.exe

RUN powershell -Command \
    Invoke-WebRequest 'https://dot.net/v1/dotnet-install.ps1' -OutFile 'dotnet-install.ps1'; \
    ./dotnet-install.ps1 -Channel 8.0 -InstallDir 'C:\Program Files\dotnet'; \
    Remove-Item 'dotnet-install.ps1'

RUN Invoke-WebRequest -Uri "https://download.microsoft.com/download/70ad2c3b-761f-4aa9-a9de-e7405aa2b4c1/ArtifactSigningClientTools.msi" -OutFile .\ArtifactSigningClientTools.msi; \
    Start-Process msiexec.exe -Wait -ArgumentList '/I ArtifactSigningClientTools.msi /quiet'; \
    Remove-Item .\ArtifactSigningClientTools.msi

ENV GAMS_SYS_DIR=C:/gams

RUN choco install -y --ignore-checksums googlechrome

RUN pip install gamspy

ENV GAMSPY_INTERPRETER_PATH=python
