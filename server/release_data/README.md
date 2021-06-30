# Installation
GAMS MIRO Server makes use of GAMS Engine, a server software that allows you to run GAMS jobs in cloud environments. 

1\.  Engine
   1.  For the installation of GAMS Engine follow the instructions at [https://www.gams.com/engine/installation.html](https://www.gams.com/engine/installation.html). When successfully installed, MIRO Engine will be mounted on `/engine` (Management interface) and `/engine/api` (Engine API).  
   2.  Log into the Engine user interface (`/engine`) with username: `admin` and password: `admin`. Change the password of the administrator via the interface.   
   
2\.  MIRO Server
   1.  Extract `miro_server.zip`. This will create a subfolder `miro_server`.
   2.  Open a terminal and `cd` into that folder.
   3.  In case MIRO Server and GAMS Engine run on the same machine: 
       Get IP of `docker0` network interface via: `sudo ip addr show docker0 |grep -Po 'inet \K[\d.]+'`.
   4.  Install MIRO Sever by running `./miro-server install`. You will be asked to enter the GAMS Engine host and the [namespace](https://gams.com/engine/administration.html#namespaces) to be used by MIRO Server. For the host enter: `http://<IP extracted from previous step>/engine/api`. If you specified a port when installing GAMS Engine, this port must also be specified here: `http://<IP extracted from previous step>:<engine port>/engine/api`. The default namespace is `global`. You can also specify any other namespace created in GAMS Engine. You can change the GAMS Engine connection info at any time by modifying the `.env` file that is located inside the miro_server directory. The following environment variables have to be set:

   |Environment variable name|Environment variable value| Example |
   |-------------------------|--------------------------|---------|
   | `GMS_MIRO_ENGINE_HOST`    | IP adress and port/DNS of Engine|`https://miro.gams.com/engine/api`|
   | `GMS_MIRO_ENGINE_NS `  | Engine namespace to be used by MIRO Server | `miro_server`|
   
    Note that MIRO Server must be restarted if the `.env` has been changed.
    You will also be asked whether you want to use MIRO Server with or without authentication service, i.e. whether users have to log in with an existing GAMS Engine account (default) or whether everyone with a URL should have free access. Read more about the user management in the section `Authentication and User Management`.
    The installation script will inform you whether the installation was successful or not.

# Start GAMS MIRO Server
Once you have GAMS MIRO Server installed, you can launch it via `./miro-server start`. GAMS MIRO Server will now listen on port 8080. You can log in with any GAMS Engine user that has at least execute permissions on the namespace provided. A user with full access to that namespace will be considered as administrator by MIRO Server and can add/remove applications. To access the admin panel, go to `/app/admin`. Add your MIRO applications here. To access a MIRO application that has been added to MIRO Server, go to `/app_direct/<appId>`, where `appId` is the lowercase name of your main gms file.

# Update GAMS MIRO Server
To update GAMS MIRO Server to the latest version, run `./miro-server update`. Note that this will pull new images and launch them. If you only want to pull new images, run `./miro-server pull`.

# Stop GAMS MIRO Server
To stop a running instance of GAMS MIRO Server, run `./miro-server stop`.

# Uninstall GAMS MIRO Server
To remove GAMS MIRO Server including all data associated with it from your server, run `./miro-server uninstall`. Additionally you can remove the directory where you extracted the configuration files of GAMS MIRO server.

# Nginx example configuration
We suggest you run GAMS MIRO Server behind a reverse proxy such as nginx. You can find an example configuration in the file `miro.conf` located inside this directory. To apply this configuration, simply copy it to the configuration location of your nginx (e.g. `sudo cp miro.conf /etc/nginx/conf.d/`). Note that you will have to reload nginx afterwards using `sudo nginx -s reload`.

# Hosting GAMS Engine and MIRO Server on the same host
When you are hosting MIRO Engie and MIRO Server on the same host, you have to merge the nginx configurations of both GAMS Engine and MIRO Server. A resulting `/etc/nginx/conf.d/miro.conf` could look as follows:

```
map $http_upgrade $connection_upgrade {
   default upgrade;
   ''      close;
}
# redirect http to https (optional)
#server {
#    listen 80;
#    return 301 https://$host$request_uri;
#}
server {
# SSL setup (optional)
#    listen 443 ssl;
#    ssl_certificate /path/to/certificate/file.pem;
#    ssl_certificate_key /path/to/key/file.key.pem;
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
        proxy_pass http://127.0.0.1:5000/engine;
        
        proxy_redirect    off;
        proxy_set_header  Host             $http_host;
        proxy_set_header  X-Real-IP        $remote_addr;
        proxy_set_header  X-Forwarded-For  $proxy_add_x_forwarded_for;
        proxy_set_header  X-Forwarded-Proto $scheme;

    }
    client_max_body_size 0;
}
```

Note that even though both GAMS Engine and MIRO Server run on the same host, the GAMS Engine host is not `localhost`. In case TLS, port/certificate info etc. is used, this needs to be added here as well. If you need help with this, contact us. Nginx needs to be reloaded if the configuration has been changed: ` sudo nginx -s reload`.

# Host MIRO Server on a different path than root
You may want to host MIRO Server on a different path. To do this, you must adjust the context path in the file 'application.yml' accordingly (`server.servlet.context-path`). This path must be identical to the location specified in the nginx configuration.

# Extending the MIRO Docker image
In case your MIRO applications need additional packages, you have to extend the [MIRO UI Docker image](https://github.com/GAMS-dev/miro/tree/master/server/ui/Dockerfile). You can do so by adding the additional packages required by your custom renderers to the file `additional_packages` located inside this directory. Each package name must be on a new line. Once all packages are added to this file, run `./miro-server build`.
Please note that additional packages may cause version conflicts with packages used internally by MIRO. We therefore recommend trying to keep the number of additional packages to a minimum.

# Authentication and User Management
By default MIRO Server uses the authentication service of GAMS Engine. Every user who wants to solve GAMS jobs with MIRO Server needs a GAMS Engine user account with at least execute permissions in the [namespace](https://gams.com/engine/administration.html#namespaces) to be used. If you are GAMS Engine administrator, you can manage users and namespaces for MIRO Server directly in the [Engine UI](https://www.gams.com/engine/administration.html#user-management).
> If you decide to use MIRO Server without authentication (e.g. to showcase applications as in the case of the [MIRO Gallery](https://miro.gams.com)), you need to register one GAMS Engine user with execute permissions in the namespace to be used, e.g. a user `anonymous`. You will need the credentials of that user in step 2.4. You can also provide the credentials of this user via the `.env` file:

|Environment variable name|Environment variable value| Example |
|-------------------------|--------------------------|---------|
| `GMS_MIRO_ENGINE_ANONYMOUS_USER`    | Username of anonymous user |`miro_server_anonymous`|
| `GMS_MIRO_ENGINE_ANONYMOUS_PWD `  | Password of anonymous user | `t@qHwt%3Mh`|

# MIRO server admin panel
The admin panel can be reached at: `https://your-miro-server-domain.com/app_direct/admin`. You can log in with any Engine user that has write permissions on your MIRO Server namespace (provided via `GMS_MIRO_ENGINE_NS `).

# Running under SELinux
Note that with SELinux active (e.g. CentOS/RHEL), you might have to allow your nginx server to proxy to the upstream MIRO Server host. You can do so by running: `setsebool -P httpd_can_network_connect 1`.
