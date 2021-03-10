# Installation
GAMS MIRO Server makes use of GAMS Engine, a server software that allows you to run GAMS jobs in cloud environments. 

1\.  Engine
   1.  For the installation of GAMS Engine follow the instructions at [https://www.gams.com/engine/installation.html](https://www.gams.com/engine/installation.html). When successfully installed, MIRO Engine will be mounted on `/engine` (Management interface) and `/engine/api` (Engine API).  
   2.  Log into the management interface (`/engine`) with username: `admin` and password: `admin`. Change the password of the administrator via the interface.
   
2\.  MIRO Server
   1.  Extract `miro_server.zip`. This will create a subfolder `miro_server`.
   2.  Open a terminal and cd into that folder.
   3.  Get IP of `docker0` network interface via: `sudo ip addr show docker0 |grep -Po 'inet \K[\d.]+'`.
   4.  Install MIRO Sever by running `./miro-compose install`. You will be asked to enter the GAMS Engine host and the [namespace](https://gams.com/engine/administration.html#namespaces) to be used by MIRO Server. For the host enter: `<IP extracted from previous step>/engine/api`. If you specified a port when installing GAMS Engine, this port must also be specified here: `<IP extracted from previous step>:<engine port>/engine/api`. The default namespace is `global`. You can also specify any other namespace created in GAMS Engine. You can change the GAMS Engine connection info at any time by modifying the `.env` file that is located inside the miro_server directory. The following environment variables have to be set:

   |Environment variable name|Environment variable value| Example |
   |-------------------------|--------------------------|---------|
   | `GMS_MIRO_ENGINE_HOST`    | IP adress and port/DNS of Engine|`https://miro.gams.com/engine/api`|
   | `GMS_MIRO_ENGINE_NS `  | Engine namespace to be used by MIRO Server | `miro_server`|
   
   Note that MIRO Server must be restarted if the `.env` has been changed.
   The installation script will inform you whether the installation was successful or not.
   
# Start GAMS MIRO Server
Once you have GAMS MIRO Server installed, you can launch it via `./miro-compose start`. GAMS MIRO Server will now listen on port 8080. You can log in with any GAMS Engine user that has at least execute permissions on the namespace provided. A user with full access to that namespace will be considered as administrator by MIRO Server and can add/remove applications. To access the admin panel, go to `/app/admin`. Add your MIRO applications here. To access a MIRO application that has been added to MIRO Server, go to `/app_direct/<appId>`, where `appId` is the lowercase name of your main gms file.

# Update GAMS MIRO Server
To update GAMS MIRO Server to the latest version, run `./miro-compose update`. Note that this will pull new images and launch them. If you only want to pull new images, run `./miro-compose pull`.

# Stop GAMS MIRO Server
To stop a running instance of GAMS MIRO Server, run `./miro-compose stop`.

# Uninstall GAMS MIRO Server
To remove GAMS MIRO Server including all data associated with it from your server, run `./miro-compose uninstall`. Additionally you can remove the directory where you extracted the configuration files of GAMS MIRO server.

# Nginx example configuration
We suggest you run GAMS MIRO Server behind a reverse proxy such as nginx. You can find an example configuration in the file `miro.conf` located inside this directory. To apply this configuration, simply copy it to the configuration location of your nginx (e.g. `sudo cp miro.conf /etc/nginx/conf.d/`). Note that you will have to reload nginx afterwards using `sudo nginx -s reload`.

# Hosting GAMS Engine and MIRO Server on the same host
When you are hosting MIRO Engie and MIRO Server on the same host, you have to merge the nginx configurations of both GAMS Engine and MIRO Server. A resulting `/etc/nginx/conf.d/miro.conf` could look as follows:

```
map $http_upgrade $connection_upgrade {
   default upgrade;
   ''      close;
}
server {
    listen 80;
    location / {
        proxy_pass http://127.0.0.1:8080;

        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 600s;

        proxy_redirect    off;
        proxy_set_header  Host             $http_host;
        proxy_set_header  X-Real-IP        $remote_addr;
        proxy_set_header  X-Forwarded-For  $proxy_add_x_forwarded_for;
        proxy_set_header  X-Forwarded-Proto $scheme;
    }
    location /engine {
        proxy_pass http://127.0.0.1:5000;
    }
    client_max_body_size 0;
}
```

Note that even though both GAMS Engine and MIRO Server run on the same host, the GAMS Engine host is not `localhost`. In case TLS, port/certificate info etc. is used, this needs to be added here as well. If you need help with this, contact us. Nginx needs to be reloaded if the configuration has been changed: ` sudo nginx -s reload`.

# Host MIRO Server on a different path than root
You may want to host MIRO Server on a different path. To do this, you must adjust the context path in the file 'application.yml' accordingly (`server.servlet.context-path`). This path must be identical to the location specified in the nginx configuration.
Note that with SELinux active (e.g. CentOS/RHEL), you have to allow your nginx server to proxy to the upstream MIRO Server host. You can do so by running: `setsebool -P httpd_can_network_connect 1`.

# Extending the MIRO Docker image
In case your MIRO applications need additional packages, you have to extend the Docker UI image (https://github.com/GAMS-dev/miro_desktop/blob/master/Dockerfile). You can do so by adding the additional packages required by your custom renderers to the file `additional_packages` located inside this directory. Each package name must be on a new line. Once all packages are added to this file, run `./miro-compose build`. 

# Using no authentication
When using no authentication (e.g. to showcase applications as in the case of the [MIRO Gallery](https://miro.gams.com)), you need to provide the credentials of some Engine user with execute permissions via the `.env` file:

|Environment variable name|Environment variable value| Example |
|-------------------------|--------------------------|---------|
| `GMS_MIRO_ENGINE_ANONYMOUS_USER`    | Username of anonymous user |`miro_server_anonymous`|
| `GMS_MIRO_ENGINE_ANONYMOUS_PWD `  | Password of anonymous user | `t@qHwt%3Mh`|

The admin panel can be reached at: `https://your-miro-server-domain.com/app_direct/admin`. You can log in with any Engine user that has write permissions on your MIRO Server namespace (provided via `GMS_MIRO_ENGINE_NS `).
