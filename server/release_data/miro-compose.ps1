param([string]$action)

function install
{
    if (!(Test-Path .env)) {
        "No env file found. Please report to GAMS!"
        exit 1
    }

    if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_HOST" -SimpleMatch)) {
        $ENGINE_HOST = Read-Host -Prompt "Please enter GAMS MIRO Engine host: "
        if (!$ENGINE_HOST) {
            "Invalid GAMS MIRO Engine host!"
            exit 1
        }
        Add-Content -Value "GMS_MIRO_ENGINE_HOST=$ENGINE_HOST" .env
    }
    if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_USER" -SimpleMatch)) {
        $ENGINE_USER = Read-Host -Prompt "Please enter GAMS MIRO Engine username (at least inviter role required): "
        if (!$ENGINE_USER) {
            "Invalid GAMS MIRO Engine username!"
            exit 1
        }
        Add-Content -Value "GMS_MIRO_ENGINE_USER=$ENGINE_USER" .env
    }
    if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_PWD" -SimpleMatch)) {
        $ENGINE_PASS = Read-Host -Prompt "Please enter the password of this user: " -AsSecureString
        if (!$ENGINE_PASS) {
            "Invalid GAMS MIRO Engine password!"
            exit 1
        }
        Add-Content -Value "GMS_MIRO_ENGINE_PWD=$ENGINE_PASS" .env
    }
    if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_NS" -SimpleMatch)) {
        $ENGINE_NS = Read-Host -Prompt "Please enter namespace where this user is inviter on: "
        if (!$ENGINE_NS) {
            "Invalid GAMS MIRO Engine namespace!"
            exit 1
        }
        Add-Content -Value "GMS_MIRO_ENGINE_NS=$ENGINE_NS" .env
    }

    'You can change the GAMS MIRO Engine credentials at any time by modifying the ".env" file.'

    "Installing GAMS MIRO server. Please wait..."

    pull_images

    if (!(Test-Path "logs")) {
        mkdir logs
    }

    if (!(Test-Path "data")) {
        mkdir data
    }
    Copy-Item data_raw\* -Destination data -Recurse -Force | Out-Null
    if (!$?) {
        'Failed to copy data files from "data_raw" to "data". Check your permissions.'
        exit 1
    }

    if (!(Test-Path "models")) {
        mkdir models
    }

    "GAMS MIRO server installed successfully! Use './miro-compose.ps1 start' to start now."
}

function install
{
    "Building custom MIRO Docker image"
    docker build -t hub.gams.com/gamsmiro-ui:latest -f Dockerfile-extend-miro .
    if (!$?) { exit 1 }
}

function pull_images
{
    "Pulling Docker images for GAMS MIRO Server"
    docker-compose -f docker-compose.yml pull
    if (!$?) { exit 1 }
}

function launch
{
    "Starting GAMS MIRO server"
    docker-compose -f docker-compose.yml up -d
    if (!$?) { exit 1 }
    "GAMS MIRO server started."
}

function stop_proxies
{
    $orphaned_adminc = docker container ls -f "network=miroserver-network" -f "ancestor=hub.gams.com/gamsmiro-admin" --format "{{.ID}}"
    if ($orphaned_adminc) {
        docker stop $orphaned_adminc | Out-Null
        docker rm $orphaned_adminc | Out-Null
    }
    
    $orphaned_uic = docker container ls -f "network=miroserver-network" -f "ancestor=hub.gams.com/gamsmiro-ui" --format "{{.ID}}"
    if ($orphaned_uic) {
        docker stop $orphaned_uic | Out-Null
        docker rm $orphaned_uic | Out-Null
    }
}

function stop
{
    "Stopping GAMS MIRO server"
    stop_proxies
    & docker-compose -f docker-compose.yml down
    if (!$?) { exit 1 }
    "GAMS MIRO server stopped."
}

function restart
{
    "Restarting GAMS MIRO server"
    docker-compose -f docker-compose.yml restart
    if (!$?) { exit 1 }
    "GAMS MIRO server restarted."
}

function uninstall
{
    "Uninstalling GAMS MIRO server"
    $uninstall_confirm = Read-Host -Prompt "Are you sure you want to remove all data of GAMS MIRO server? This cannot be undone! (yes)"

    if ($uninstall_confirm -ne "yes") {
        echo "Uninstalling GAMS MIRO Server was interrupted."
        exit 0
    }
    stop
    docker-compose -f docker-compose.yml down -v
    if (!$?) { exit 1 }
    Remove-Item data,models -Recurse -Confirm:$false -ErrorAction SilentlyContinue
    if (!$?) {
        'Could not remove "data" and/or "models" directory. Check your permissions'
        exit 1
    }
    "GAMS MIRO server uninstalled."
}

switch -Exact ($action)
{
    "stop" {
        stop
        break
    }
    "uninstall" {
        uninstall
        break
    }
    "restart" {
        restart
        break
    }
    "start" {
        launch
        break
    }
    "install" {
        install
        break
    }
    "build" {
        build
        break
    }
    "pull" {
        pull_images
        break
    }
    "update" {
        pull_images
        launch
        break
    }
    Default {
        "Usage: {0} install|build|start|stop|restart|pull|update|uninstall" -f $MyInvocation.MyCommand.Name
        exit 1
    }
}

exit 0