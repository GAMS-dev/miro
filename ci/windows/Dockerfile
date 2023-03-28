FROM mcr.microsoft.com/windows/server:ltsc2022

SHELL ["powershell", "-executionpolicy", "bypass", "-command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue'; $verbosePreference='Continue'; Set-PSDebug -Trace 0;"]

ARG NODE_VERSION=18.15.0
ENV NODE_VERSION=${NODE_VERSION}

ARG GPG_VERSION=4.1.0
ENV GPG_VERSION=${GPG_VERSION}

ARG YARN_VERSION=1.22.19
ENV YARN_VERSION=${YARN_VERSION}

ARG R_VERSION=4.2.3
ENV R_VERSION=${R_VERSION}

ARG INNOEXTRACT_VERSION=1.9
ENV INNOEXTRACT_VERSION=${INNOEXTRACT_VERSION}

ARG GAMS_VERSION=42.4.0
ENV GAMS_VERSION=${GAMS_VERSION}

RUN Invoke-WebRequest $('https://files.gpg4win.org/gpg4win-{0}.exe' -f $env:GPG_VERSION) -OutFile 'gpg4win.exe' -UseBasicParsing ; \
    Start-Process .\gpg4win.exe -ArgumentList '/S' -NoNewWindow -Wait ;

RUN @( \
    '94AE36675C464D64BAFA68DD7434390BDBE9B9C5', \
    'FD3A5288F042B6850C66B31F09FE44734EB7990E', \
    '71DCFD284A79C3B38668286BC97EC7A07EDE3FC1', \
    'DD8F2338BAE7501E3DD5AC78C273792F7D83545D', \
    'C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8', \
    'B9AE9905FFD7803F25714661B63B535A4C206CA9', \
    '56730D5401028683275BD23C23EFEFE93C4CFFFE', \
    '77984A986EBC2AA786BC0F66B01FBB92821C587A', \
    '8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600' \
    ) | foreach { \
    gpg --keyserver ha.pool.sks-keyservers.net --recv-keys $_ ; \
    } ; \
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 ; \
    Invoke-WebRequest $('https://nodejs.org/dist/v{0}/SHASUMS256.txt.asc' -f $env:NODE_VERSION) -OutFile 'SHASUMS256.txt.asc' -UseBasicParsing ; \
    gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc ; \
    Invoke-WebRequest $('https://nodejs.org/dist/v{0}/node-v{0}-win-x64.zip' -f $env:NODE_VERSION) -OutFile 'node.zip' -UseBasicParsing ; \
    $sum = $(cat SHASUMS256.txt.asc | sls $('  node-v{0}-win-x64.zip' -f $env:NODE_VERSION)) -Split ' ' ; \
    if ((Get-FileHash node.zip -Algorithm sha256).Hash -ne $sum[0]) { Write-Error 'SHA256 mismatch' } ; \
    Expand-Archive node.zip -DestinationPath C:\ ; \
    Rename-Item -Path $('C:\node-v{0}-win-x64' -f $env:NODE_VERSION) -NewName 'C:\nodejs'

RUN [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 ; \
    Invoke-WebRequest $('https://yarnpkg.com/downloads/{0}/yarn-{0}.msi' -f $env:YARN_VERSION) -OutFile yarn.msi -UseBasicParsing ; \
    $sig = Get-AuthenticodeSignature -filepath yarn.msi ; \
    if ($sig.Status -ne 'Valid') { Write-Error 'Authenticode signature is not valid' } ; \
    Start-Process msiexec.exe -ArgumentList '/i', 'yarn.msi', '/quiet', '/norestart' -NoNewWindow -Wait

RUN New-Item $($env:APPDATA + '\npm') -Type Directory ; \
    $env:PATH = 'C:\nodejs;{0}\npm;C:\yarn\bin;{1}' -f $env:APPDATA, $env:PATH ; \
    Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\' -Name Path -Value $env:PATH

RUN Invoke-WebRequest "https://cran.r-project.org/bin/windows/Rtools/rtools42/files/rtools42-5355-5357.exe" -OutFile "Rtools.exe" -UseBasicParsing ; \
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

ENV GAMS_SYS_DIR=C:/gams

RUN iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1')) ; \
    choco install -y googlechrome
