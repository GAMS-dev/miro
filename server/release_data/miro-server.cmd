@echo off
set COMMAND=
:paramloop
if x%1 == x goto usage
if %1 == -h goto usage
if %1 == /h goto usage
if %1 == install set COMMAND=%1
if %1 == build set COMMAND=%1
if "%1" == "start" set COMMAND=%~1
if %1 == stop set COMMAND=%1
if %1 == restart set COMMAND=%1
if %1 == pull set COMMAND=%1
if %1 == update set COMMAND=%1
if %1 == uninstall set COMMAND=%1
if not x%2 == x goto usage
if x%2 == x goto cont
shift
goto paramloop
:cont
if not defined COMMAND (
   goto usage
)
PowerShell -ExecutionPolicy UnRestricted -File .\miro-server.ps1 %COMMAND%
exit /b
:usage
echo "Use: miro-server.cmd install|build|start|stop|restart|pull|update|uninstall"
if not x%1 == x-h exit /b 1
