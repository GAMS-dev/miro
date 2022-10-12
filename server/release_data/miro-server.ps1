param([string]$action)

function install
{
    if (!(Test-Path .env) -or !(type .env | Select-String -Pattern "GMS_MIRO_DATABASE_PWD" -SimpleMatch)) {
        [Reflection.Assembly]::LoadWithPartialName("System.Web") | out-null
        $db_password = [System.Web.Security.Membership]::GeneratePassword(40,0)
		$pwd_docker = "$pwd" -replace "\\", "/"
		$drive, $pwd_docker = "$pwd_docker".Split(':')
		$pwd_docker = "/" + $drive.toLower() + $pwd_docker
        "A new master password for your MIRO Server database was generated: $db_password"
        'Please keep this password in a safe place.'
        Add-Content -Value "PWD=$pwd_docker`r`nGMS_MIRO_DATABASE_PWD=$db_password" .env
    }

    if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_HOST" -SimpleMatch)) {
        $ENGINE_HOST = Read-Host -Prompt "Please enter GAMS Engine host"
        if (!$ENGINE_HOST) {
            "Invalid GAMS Engine host!"
            exit 1
        }
        Add-Content -Value "GMS_MIRO_ENGINE_HOST=$ENGINE_HOST" .env
    }
    if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_NS" -SimpleMatch)) {
        $ENGINE_NS = Read-Host -Prompt "Please specify the namespace to be connected to MIRO Server: "
        if (!$ENGINE_NS) {
            "Invalid GAMS Engine namespace!"
            exit 1
        }
        Add-Content -Value "GMS_MIRO_ENGINE_NS=$ENGINE_NS" .env
    }

    'You can change the GAMS Engine connection info at any time by modifying the ".env" file.'
    'Note that MIRO Server must be restarted for these changes to take effect.'

    $ENABLE_AUTH = Read-Host -Prompt "Enable authentication? [Y/n]"

    $VALID_NO_ANSWERS = @("n","N","no","NO","nO","No")
    if ($ENABLE_AUTH -in $VALID_NO_ANSWERS) {
        (Get-Content -Path data_raw/application.yml) -replace "authentication: .*", "authentication: none" | Set-Content data_raw/application.yml
        if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_ANONYMOUS_USER" -SimpleMatch)) {
            $ENGINE_ANON_USR = Read-Host -Prompt "Please enter name of user to be used to run jobs"
            if (!$ENGINE_ANON_USR) {
                "Invalid username!"
                exit 1
            }
            Add-Content -Value "GMS_MIRO_ENGINE_ANONYMOUS_USER=$ENGINE_ANON_USR" .env
        }
        if (!(type .env | Select-String -Pattern "GMS_MIRO_ENGINE_ANONYMOUS_PWD" -SimpleMatch)) {
            $ENGINE_ANON_PWD = Read-Host -Prompt "Please enter password of this user" -AsSecureString
            if (!$ENGINE_ANON_PWD) {
                "Invalid password!"
                exit 1
            }
            Add-Content -Value "GMS_MIRO_ENGINE_ANONYMOUS_PWD=$ENGINE_ANON_PWD" .env
        }
    } else {
        (Get-Content -Path data_raw/application.yml) -replace "authentication: .*", "authentication: webservice" | Set-Content data_raw/application.yml
    }

    $current_dir_name = Split-Path -Path "$pwd" -Leaf -Resolve
    $miro_server_network_name="${CURRENT_DIR_NAME}-network"

    (Get-Content -Path data_raw/application.yml) -replace "container-network: .*", "container-network: $miro_server_network_name" | Set-Content data_raw/application.yml

    $start_replace = $false
    (Get-Content docker-compose.yml) |
    Foreach-Object {
        if ($start_replace -eq $true){
            $start_replace= $false
            if ($_ -match " name:") {
            "    name: $miro_server_network_name"
            } else {
                $_
            }
        }
        elseif($_ -match " gamsmiro-network:"){
            $start_replace = $true
            $_
        }
        else {
            $_
        }
    } | Set-Content docker-compose.yml

    "Installing GAMS MIRO Server. Please wait..."

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

    "GAMS MIRO Server installed successfully! Use './miro-server.cmd start' to start now."
}

function build
{
    "Building custom MIRO Docker image"
    docker build --progress=plain -t gams/miro-ui:latest -f Dockerfile-extend-miro .
    if (!$?) { exit 1 }
}

function pull_images
{
    "Pulling Docker images for GAMS MIRO Server"
    docker-compose -f docker-compose.yml pull
    if (!$?) { exit 1 }
}

function update
{
    "Updating GAMS MIRO Server"
    $BUILD_UI_IMAGE = 0

    if (![String]::IsNullOrWhiteSpace((Get-Content -Path additional_packages))) {
        $REBUILD_UI_IMAGE = Read-Host -Prompt "It looks like you are using a custom MIRO image with additional packages. Do you want to recreate this image as part of the update? [Y/n]"

        $VALID_NO_ANSWERS = @("n","N","no","NO","nO","No")
        if ($REBUILD_UI_IMAGE -in $VALID_NO_ANSWERS) {
            $BUILD_UI_IMAGE = 0
        } else {
            $BUILD_UI_IMAGE = 1
        }
    }
    pull_images
    if ($BUILD_UI_IMAGE) {
        build
    }
    launch
}

function launch
{
    "Starting GAMS MIRO Server"
    docker-compose -f docker-compose.yml up -d
    if (!$?) { exit 1 }
    "GAMS MIRO Server started."
}

function stop_proxies
{
    $orphaned_adminc = docker container ls -f "network=miroserver-network" -f "ancestor=gams/miro-admin" --format "{{.ID}}"
    if ($orphaned_adminc) {
        docker stop $orphaned_adminc | Out-Null
        docker rm $orphaned_adminc | Out-Null
    }

    $orphaned_uic = docker container ls -f "network=miroserver-network" -f "ancestor=gams/miro-ui" --format "{{.ID}}"
    if ($orphaned_uic) {
        docker stop $orphaned_uic | Out-Null
        docker rm $orphaned_uic | Out-Null
    }
}

function stop
{
    "Stopping GAMS MIRO Server"
    stop_proxies
    & docker-compose -f docker-compose.yml down
    if (!$?) { exit 1 }
    "GAMS MIRO Server stopped."
}

function restart
{
    "Restarting GAMS MIRO Server"
    docker-compose -f docker-compose.yml restart
    if (!$?) { exit 1 }
    "GAMS MIRO Server restarted."
}

function uninstall
{
    "Uninstalling GAMS MIRO Server"
    $uninstall_confirm = Read-Host -Prompt "Are you sure you want to remove all data of GAMS MIRO Server? This cannot be undone! (yes)"

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
    "GAMS MIRO Server uninstalled."
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
        update
        break
    }
    Default {
        "Usage: {0} install|build|start|stop|restart|pull|update|uninstall" -f $MyInvocation.MyCommand.Name
        exit 1
    }
}

exit 0
