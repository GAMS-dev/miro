# Installation
GAMS MIRO Server requires Docker as well as Docker Compose to run. Once these prerequisites are installed, you have to login to the GAMS Docker registry. To do so, run `docker login hub.gams.com` with the credentials GAMS provided you with. Afterward, you are ready to install MIRO Server by running: `./miro-compose install`. The installation script will inform you whether the installation was successful or not.
GAMS MIRO Server requires an instance of GAMS Engine to solve the GAMS jobs. You have to provide the host and namespace to be used by MIRO Server via environment variables.
You can either set these variables inside the shell you start GAMS MIRO Server from or append them to the `.env` file that is located inside this directory. The following environment variables have to be set:

|Environment variable name|Environment variable value| Example |
|-------------------------|--------------------------|---------|
| `GMS_MIRO_ENGINE_HOST`    | IP adress and port/DNS of Engine|`https://miro.gams.com/engine/api`|
| `GMS_MIRO_ENGINE_NS `  | Engine namespace to be used by MIRO Server | `miro_server`|

# Start GAMS MIRO Server
Once you have GAMS MIRO Server installed, you can launch it via `./miro-compose start`. GAMS MIRO Server will now listen on port 8080. You can log in with any user that has at least execute permissions on the namespace provided. A user with full access to that namespace will be considered as administrator by MIRO Server and can add/remove applications.

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

Note that even though both GAMS Engine and MIRO Server run on the same host, the GAMS Engine host is not `localhost`.  

# Host MIRO Server on a different path than root
You may want to host MIRO Server on a different path. To do this, you must adjust the context path in the file 'application.yml' accordingly (`server.servlet.context-path`).
Note that with SELinux active (e.g. CentOS/RHEL), you have to allow your nginx server to proxy to the upstream MIRO Server host. You can do so by running: `setsebool -P httpd_can_network_connect 1`.

# Extending the MIRO Docker image
In case your MIRO applications need additional packages, you have to extend the MIRO Docker image (https://github.com/GAMS-dev/miro_desktop/blob/master/Dockerfile). You can do so by adding the additional packages required by your custom renderers to the file `additional_packages` located inside this directory. Each package name must be on a new line. Once all packages are added to this file, run `./miro-compose build`.

# Using no authentication
When using no authentication (e.g. to showcase applications as in the case of the [MIRO Gallery](https://miro.gams.com)), you can reach the admin panel at: `https://your-miro-server-domain.com/app_direct/admin`. You can log in with any Engine user that has write permissions on your MIRO Server namespace (`GMS_MIRO_ENGINE_NS `).
