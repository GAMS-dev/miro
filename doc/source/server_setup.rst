Server Setup
============


.. _server-install:

GAMS MIRO Server - Setup
------------------------



.. admonition:: Note
   :class: warning

   The following instructions refer exclusively to the installation and configuration of GAMS MIRO Server. The corresponding steps have to be performed on the server used for this purpose. Please make sure that you have installed GAMS Engine before proceeding with the installation of GAMS MIRO Server.


To install MIRO Server, first download the ZIP file that contains everything you need to get started `here <./download.html#miro-server>`__.

The general installation instructions are given below. However, keep in mind that the installation is highly dependent on your setup. The setup of GAMS MIRO Server should typically be performed by a system administrator. If you have problems with the installation, please contact us at `support@gams.com <mailto:support@gams.com>`__.

.. admonition:: Note
   :class: warning

   GAMS Engine is currently only supported on x86_64 Linux distributions that are `supported by Docker <https://docs.docker.com/engine/install/#supported-platforms>`__!


#. Install Docker from https://docs.docker.com/engine/install/. Start Docker afterwards. We also recommend configuring Docker to `start on boot <https://docs.docker.com/engine/install/linux-postinstall/#configure-docker-to-start-on-boot>`__. To use the Docker CLI without *sudo*, you must `add your user to the docker group <https://docs.docker.com/install/linux/linux-postinstall/#manage-docker-as-a-non-root-user>`__.

#. Install the Docker Compose plugin from https://docs.docker.com/compose/install/. The standalone ``docker-compose`` binary is also supported.

.. raw:: html

   <span id="miro_server"></span>

#. Extract ``miro_server.zip``. This will create a subfolder ``miro_server``.

#. Open a terminal and ``cd`` into that folder.

   .. tip::

      GAMS MIRO applications will later be stored at this location.


#. If MIRO Server and GAMS Engine run on the same machine, get the IP of the ``docker0`` network interface: ``sudo ip addr show docker0 |grep -Po 'inet \K[\d.]+'``. You will need this IP in the next step.

.. raw:: html

   <span id="step6"></span>

#. Install MIRO Server by running ``./miro-server install``. You will be asked to enter the GAMS Engine host and the `namespace <https://gams.com/engine/administration.html#namespaces>`__ to be used by MIRO Server.

   - If MIRO Server and GAMS Engine are running on the same machine, enter the following host: ``http://<IP extracted from previous step>/<engine location>/api``. The *Engine location* is root/empty by default. Since the MIRO Server location is also root/empty by default, make sure that the two services do not conflict (i.e. one of them must use a different location).
     If you specified a port when installing GAMS Engine, this port must also be specified here: ``http://<IP extracted from previous step>:<engine port>/<engine location>/api``.
   - If GAMS Engine is running on a separate machine, enter the URL here, e.g. ``http(s)://mydomain.com/api``.

   The default namespace is ``global``. You can also specify any other namespace created in GAMS Engine. You can change the GAMS Engine connection info at any time by modifying the auto-generated ``.env`` file that is located inside the miro_server directory. The following environment variables have to be set:

   .. container:: table-responsive

      +---------------------------+--------------------------------------------+--------------------------+
      | Environment Variable Name | Environment Variable Value                 | Example                  |
      +===========================+============================================+==========================+
      | GMS_MIRO_ENGINE_HOST      | IP address and port/DNS of Engine          | https://mydomain.com/api |
      +---------------------------+--------------------------------------------+--------------------------+
      | GMS_MIRO_ENGINE_NS        | Engine namespace to be used by MIRO Server | miro_server              |
      +---------------------------+--------------------------------------------+--------------------------+

   If you change the ``.env`` file, run ``./miro-server stop`` followed by ``./miro-server start`` to apply the changes.

   You will also be asked whether you want to use MIRO Server with or without authentication service, i.e. whether users have to log in with an existing GAMS Engine account (default) or whether everyone with the valid URL should have free access to the app(s). Read more about the user management `here <./server_administration.html#authentication-user-management>`__.

   The installation script will inform you whether the installation was successful or not.


.. _start-stop-update-uninstall:

.. _server-interaction:

Start / Stop / Update / Uninstall
---------------------------------



**Start**

Once you have GAMS MIRO Server installed, you can launch it via

.. code-block:: bash

   > ./miro-server start

GAMS MIRO Server will now listen on port 8080 and the `MIRO Server REST API <./server_api.html>`__ on port 8081. You can log in with any GAMS Engine user who either has some permission or can see models in the specified namespace. Users without permissions in the namespace can see a model if they can see a user group associated with that model.

To directly access a MIRO application that has been added to MIRO Server, go to ``/app_direct/<appId>``, where ``appId`` is the lowercase name of your main GMS file by default.

**Stop**

To stop a running instance of GAMS MIRO Server, run

.. code-block:: bash

   > ./miro-server stop

.. raw:: html

   <span id="miro-server-restart"></span>

**Restart**

To perform a restart, run

.. code-block:: bash

   > ./miro-server restart

Note that this command does not apply changes to the *docker-compose.yml* or *.env* file. In this case, run ``./miro-server stop`` followed by ``./miro-server start``.

**Update**

To update GAMS MIRO Server to the latest version, run

.. code-block:: bash

   > ./miro-server update

Note that this will pull new images from `Docker Hub <https://hub.docker.com>`__ and launch them. If you have `extended the MIRO Docker image <#server-extend>`__ with additional R packages, you will be asked whether to rebuild this image as part of the update. If you only want to pull new images, run

.. code-block:: bash

   > ./miro-server pull

**Backup (Linux only)**

To back up GAMS MIRO Server, run

.. code-block:: bash

   > ./miro-server backup

This creates a new ``./backup`` directory in the current directory. A MIRO backup consists of two files: ``./backup/dump_db_${BACKUP_TIME}.gz`` and ``./backup/dump_miro_server_${BACKUP_TIME}.gz`` with a backup of the MIRO PostgreSQL database and the current directory respectively. ``BACKUP_TIME`` is a timestamp with the following format: ``%d-%m-%Y_%H_%M_%S``.

**Restore (Linux only)**

To restore GAMS MIRO Server from a backup created with the ``./miro-server backup`` command, first unzip a new ``miro_server.zip`` and navigate to the ``miro_server`` directory. Next, copy the ``backup`` directory with your backup files into this directory and run

.. code-block:: bash

   > ./miro-server restore BACKUP_TIME

where ``BACKUP_TIME`` is the backup timestamp in the form: ``%d-%m-%Y_%H_%M_%S`` (e.g. ``01-04-2022_19_13_41`` for April 1, 2022, 19:13:41).

**Uninstall**

To remove GAMS MIRO Server including all data associated with it from your server, run

.. code-block:: bash

   > ./miro-server uninstall

You must confirm by typing ``yes``. This removes the MIRO Server database as well as the *data* and *models* directories. This cannot be undone!

Additionally, you can remove the directory where you extracted the configuration files of GAMS MIRO Server.


.. _customize-miro-server:

.. _server-customize:

Customize MIRO Server
---------------------



After setting up MIRO Server, you can customize some basic settings, such as the logo on the login screen or the authentication method.

Most settings are made in the *data/application.yml* file in the *miro_server* directory. Keep the file in valid YAML format and do not change any settings other than those listed below: changes directly affect the functionality of MIRO Server!

.. dropdown:: application.yml example
   :name: application-yml

   .. code-block:: yaml

      proxy:
        title: GAMS MIRO Server
        theme: forest
        template-path: ./templates/2col
        favicon-path: ./favicon.ico
        model-dir: ${GMS_MIRO_MODEL_DIR}
        data-dir: ${GMS_MIRO_DATADIR}
        miro-lang: en
        port: 8080
        heartbeat-rate: 10000
        heartbeat-timeout: 120000
        same-site-cookie: Lax
        admin-groups: admins
        container-log-path: /logs/container-logs
        max-upload-size: 200
        force-signed-apps: false
        disable-readonly-mode: false
        anonymous-readonly-mode: false
        authentication: webservice
        openid:
          auth-url: https://login.microsoftonline.com/<your-tenant-id>/oauth2/v2.0/authorize
          token-url: https://login.microsoftonline.com/<your-tenant-id>/oauth2/v2.0/token
          jwks-url: https://login.microsoftonline.com/<your-tenant-id>/discovery/v2.0/keys
          logout-url: https://login.microsoftonline.com/common/oauth2/v2.0/logout?post_logout_redirect_uri=http%3A%2F%2Fwww.yourmiroserver.com%2Flogout-success
          client-id: xxx
          client-secret: yyy
        database:
          host: miroserver-db
          port: 5432
          name: ${GMS_MIRO_DATABASE}
          username: ${GMS_MIRO_DATABASE_USER}
          password: ${GMS_MIRO_DATABASE_PWD}
        engine:
          host: ${GMS_MIRO_ENGINE_HOST}
          ns: ${GMS_MIRO_ENGINE_NS}
          anonymous-user: ${GMS_MIRO_ENGINE_ANONYMOUS_USER}
          anonymous-pwd: ${GMS_MIRO_ENGINE_ANONYMOUS_PWD}
        docker:
          url: http://dockerproxy:2375
          internal-networking: true
          container-network: miroserver-network
          miro-image-name: ${GMS_MIRO_UI_IMAGE}
          admin-image-name: ${GMS_MIRO_ADMIN_IMAGE}

      spring:
        application.name: GAMS MIRO Server

      server:
        frame-options: sameorigin
        secure-cookies: false
        forward-headers-strategy: native
        servlet:
          session.timeout: 21600
          context-path: /

The following settings can be made in the *data/application.yml* file:

- **Title (proxy.title):**
  Sets the title that is visible in the browser tab.

.. raw:: html

   <span id="global-colortheme"></span>

- **Color theme (proxy.theme):** If not set, the default theme is used. For MIRO Server, the same `color themes <./deployment.html#sbs-customize-app>`__ are available as for MIRO Desktop:

  - Default theme (YAML value: "default")
  - Black and white ("blackandwhite")
  - Green forest ("forest")
  - Tawny ("tawny")
  - Dark blue ("darkblue")
  - Red wine ("redwine")
  - Custom theme ("custom")\*

  This global color theme setting initially applies to all apps, the app overview page and the login screen. You can set a different color theme individually for each app, which overrides the global setting. Read more about the app environment `here <./server_administration.html#app-environment>`__.

  If you have created a `custom theme for MIRO Server <./configuration_colors.html#miro-server-colors>`__, place the downloaded *colors_custom.css* file in a newly created folder in *miro_server/data*, e.g. *miro_server/data/css/colors_custom.css*. Now specify this style sheet in the *docker-compose.yml* file under 'proxy.volumes':

  .. code-block:: diff

       proxy:
         image: gams/miro-proxy:latest
         ports:
           - 8080:8080
         volumes:
     +     - ./data/css/colors_custom.css:/home/miroproxy/templates/2col/assets/css/themes/colors_custom.css
           - ./data:/home/miroproxy/data

  The path before the colon (``./data/css/colors_custom.css``) points to your custom theme CSS file. The second path points to the file inside the container that will be overwritten.

  Note that in addition to the predefined and custom themes, you can also include a `custom CSS <#custom-css>`__ file.

.. raw:: html

   <span id="global-language"></span>

- **Language (proxy.miro-lang):**

  This setting defines the global language for all deployed MIRO apps. The currently available languages are English ('en'), Chinese ('cn'), German ('de') and Spanish ('es'). This global language setting initially applies to all apps. You can still set a different language individually for each app, which overrides the global setting. This allows you to host apps in different languages. Read more about `how to change the language for an individual app. <./server_administration.html#app-environment>`__

.. raw:: html

   <span id="config-max-upload-size"></span>

- **Maximum file size when uploading (MB) (proxy.max-upload-size):**

  The maximum size (in megabytes) of uploaded files (e.g. apps in the admin panel or data in MIRO apps).

.. raw:: html

   <span id="config-enforce-signed"></span>

- **Force apps to be signed (proxy.force-signed-apps):**

  MIRO apps can be `signed <./deployment.html#sbs-sign-app>`__ to guarantee the authenticity of the developer. This can protect you from accidentally importing apps that have been tampered with by a malicious actor. By default, MIRO Server imports all apps without enforcing signature verification. To enable signature verification, set this option to *true*. MIRO will then issue an error if you try to import an app that is not signed by a trusted developer. You should store all public keys (in PEM format) of the developers you trust in the directory *miro_server/data/known_keys*.

.. raw:: html

   <span id="config-disable-readonly"></span>

- **Disable read-only mode (proxy.disable-readonly-mode):**

  By default, when a user without permissions on the MIRO Server namespace opens an application, MIRO starts in read-only mode. This means that the GAMS interaction section and the "Solve" button are hidden. In addition, the sandbox scenario cannot be saved or deleted. To disable this behavior, set this option to *true*.

.. raw:: html

   <span id="config-anonymous-readonly"></span>

- **Anonymous read-only mode (proxy.anonymous-readonly-mode):**

  Only relevant when authentication is disabled (*proxy.authentication: none*): Activates read-only mode (GAMS interaction section and "Solve" button hidden, sandbox scenario cannot be saved or deleted). We recommend not giving the `anonymous user <./server_administration.html#authentication-user-management>`__ any permissions in GAMS Engine if this setting is enabled.

.. raw:: html

   <span id="config-security-features"></span>

- | **Authentication method (proxy.authentication):**
  | When you set up MIRO Server as a server administrator, you can choose to use the service `with or without authentication <./server_administration.html#authentication-user-management>`__. This decides whether anyone with access to the MIRO Server URL is allowed to access the MIRO apps, or whether GAMS Engine credentials are used. In the *data/application.yml* file you can change this setting. The following options are available:

  - *proxy.authentication: webservice*: GAMS Engine authentication
  - *proxy.authentication: openid*: OpenID Connect authentication
  - *proxy.authentication: none*: No authentication

  When running MIRO Server without authentication, models can only be solved/submitted to GAMS Engine if a user with execute permissions is used. The credentials of this user must be provided in the *.env* file in the *miro_server* directory. More about this `here <./server_administration.html#authentication-user-management>`__.

  To set up OpenID Connect for authentication, a few additional parameters need to be set. Read `here <#server-auth-oidc>`__ for more information.

- | **Context-path (server.servlet.context-path):**
  | If you want to host MIRO Server on a different path than root, you must adjust the context path under *server.servlet.context-path*. For example, if you want to access the MIRO apps at https://mydomain.com/miroapps, then the context-path must be set to ``/miroapps``. Besides adjusting the path in the *application.yml* file, the configuration of the reverse proxy (`Nginx <#server-nginx>`__: *location* property) has to be adjusted as well. Note that this has no effect on the `MIRO Server REST API <./server_api.html>`__. To also change the root path of the REST API, you must specify the root path via the *SCRIPT_NAME* environment variable in the *docker-compose.yml* file:

  .. code-block:: diff

       auth:
         image: gams/miro-auth:latest
         ports:
           - 8081:1234
         environment:
     +     SCRIPT_NAME: /miroapps
           PORT: 1234

- **SameSite cookie policy (proxy.same-site-cookie):**

  - *Lax*: (**recommended**) provides protection against CSRF attacks
  - *Strict*: provides protection against CSRF attacks. However, note that this will block cookies when a user clicks on a link to MIRO Server. For example, assume the user is logged in to MIRO Server on https://miro-server.local and there is a link to https://miro-server.local on your intranet at https://myintranet.local. When the user clicks on this link, they will not be logged in. Therefore, we do not recommend this option unless you are aware of the consequences.

- **Secure cookies (server.secure-cookies):**
  Whether to set the *Secure* flag on all cookies (*true*) or not (*false*, the default). **Highly recommended** when using HTTPS. Secure cookies only work if MIRO Server is accessed via HTTPS.

- **X-Frame-Options header (server.frame-options):**

  - *disable*: do not set the header (i.e. allow embedding MIRO Server in any website). This is the default if the key is missing.
  - *deny*: do not allow embedding MIRO Server in any website
  - *allow-from*: allowlist of domains that may embed MIRO Server (e.g. *frame-options: allow-from https://example.com*)
  - *sameorigin*: (**recommended**, set in the default *application.yml*) allow framing if parent and child have the same origin

Other settings:

- **Logo:**
  In addition to the settings in the *application.yml*, the logo that is displayed on the login screen can also be customized. To change the logo, replace the *logo.png* under ``data/img/`` in the *miro_server* directory.

.. raw:: html

   <span id="custom-css"></span>

- | **Custom styles:**

  The CSS styles of MIRO Server can be customized. For example, you can re-design the login page or apply other custom styles to the login and app overview screens:

  .. container:: section-block

     .. image:: ../assets/images/server/custom-login-screen.png
        :alt: Custom login screen
        :class: miro-image-max-height-400pt

  For this purpose, a custom CSS file must be provided when MIRO Server is started. To do this, create a new directory ``css`` under *miro_server/data*, where you put the custom CSS (e.g. ``miro_server/data/css/styles.css``). Now specify this style sheet in the *docker-compose.yml* file under 'proxy.volumes':

  .. code-block:: diff

       proxy:
         image: gams/miro-proxy:latest
         ports:
           - 8080:8080
         volumes:
     +     - ./data/css/styles.css:/home/miroproxy/templates/2col/assets/css/styles.css
           - ./data:/home/miroproxy/data

  The path before the colon (``./data/css/styles.css``) points to your custom CSS file. The second path points to the file inside the container that will be overwritten.

  The original stylesheet used by MIRO Server can be found `here <https://github.com/GAMS-dev/miro/blob/master/server/proxy/templates/2col/assets/css/styles.css>`__.

  .. admonition:: Note
     :class: warning


     - Future MIRO Server releases may change the underlying HTML (for example, we might add, remove, or rename CSS classes). If you maintain custom stylesheets, always test them on a staging or test server before upgrading to be sure they still render as expected.
     - The styles only affect MIRO Server components and not the MIRO applications themselves!

All changes will take effect after a `restart of MIRO Server <#miro-server-restart>`__. If you change the *docker-compose.yml* file, run ``./miro-server stop`` followed by ``./miro-server start``.

.. dropdown:: Advanced: Session timeout
   :name: session-timeout

   .. container:: section-block

      When logging in to MIRO Server, an authentication token for GAMS Engine is created that is valid for 48 hours (as set in the shipped *docker-compose.yml*). When this token expires, the user must log in again, otherwise any interaction with GAMS Engine (e.g. solving a model) will result in an "Access denied" error.

      MIRO Server also uses another session token, which expires after 6 hours (independent of the Engine authentication token). This token is intended to prevent the Engine authentication token from expiring while working in a MIRO app (which would prevent the user from submitting jobs). Each time the user accesses a new page/URL within MIRO Server, e.g. by starting a MIRO app (but not while working in a MIRO app), the token is reset. If the token expires, an error message is displayed to the user, which only disappears when the user logs in again.

      Both token timeouts can be adjusted:

      - | Engine token:
        | The Engine token timeout can be set in the *docker-compose.yml* file under *services.auth.environment.SESSION_TIMEOUT* (in seconds):

        .. code:: text

           services:
             auth:
               environment:
                 SESSION_TIMEOUT: 172800

      - | MIRO Server token:
        | The MIRO Server token timeout is set in the *data/application.yml* file, key *server.servlet.session.timeout* (in seconds):

        .. code:: text

           server:
             servlet:
               session.timeout: 21600


.. _authentication-openid-connect:

.. _server-auth-oidc:

Authentication: OpenID Connect
------------------------------



GAMS MIRO Server supports OpenID Connect as an authentication method. To use OpenID Connect, the *proxy.authentication* key (in the *data/application.yml* file) needs to be set to ``openid``. In addition, the following parameters need to be specified:

- **Auth Endpoint URL (proxy.openid.auth-url):**
  The URL where OIDC initiates authentication flows. GAMS MIRO Server will redirect here when an unauthenticated user accesses a page.
- **Token Endpoint URL (proxy.openid.token-url):**
  The URL where tokens can be retrieved.
- **JSON Web Key Set (jwks) URL (proxy.openid.jwks-url):**
  The URL where the public certificates of your identity provider can be found.
- **Logout URL (proxy.openid.logout-url, optional):**
  URL that users are redirected to after signing out of MIRO Server, typically the logout endpoint (``end_session_endpoint``) of your identity provider. If you omit this option, only the user's MIRO Server session is cleared and the session at the identity provider remains active. In this case, clicking *Login* again signs the user back in without asking for credentials. See `Logging out <#server-auth-oidc-logout>`__ for details.
- **Client ID (proxy.openid.client-id):**
  A unique ID generated by your identity provider for GAMS MIRO Server.
- **Client Secret (proxy.openid.client-secret):**
  A secret generated by your identity provider for GAMS MIRO Server.
- **Use PKCE (proxy.openid.with-pkce, optional):**
  Whether to use `Proof Key for Code Exchange (PKCE) <https://tools.ietf.org/html/rfc7636>`__ (default: false).

An example for Google can be found below:

.. code:: text

   proxy:
     [...]
     authentication: openid
     openid:
       auth-url: https://accounts.google.com/o/oauth2/v2/auth
       token-url: https://oauth2.googleapis.com/token
       jwks-url: https://www.googleapis.com/oauth2/v3/certs
       logout-url: https://www.google.com/accounts/Logout?continue=https://appengine.google.com/_ah/logout?continue=http%3A%2F%2Fwww.yourmiroserver.com%2Flogout-success
       client-id: ***
       client-secret: ***

Do not forget to replace *www.yourmiroserver.com* in the *logout-url* field with the URL of your MIRO Server instance.

Similarly, an example for Microsoft Entra ID:

.. code:: text

   proxy:
     [...]
     authentication: openid
     openid:
       auth-url: https://login.microsoftonline.com/<your-tenant-id>/oauth2/v2.0/authorize
       token-url: https://login.microsoftonline.com/<your-tenant-id>/oauth2/v2.0/token
       jwks-url: https://login.microsoftonline.com/<your-tenant-id>/discovery/v2.0/keys
       logout-url: https://login.microsoftonline.com/common/oauth2/v2.0/logout?post_logout_redirect_uri=http%3A%2F%2Fwww.yourmiroserver.com%2Flogout-success
       client-id: ***
       client-secret: ***

In addition, the GAMS Engine server must be `configured to use the same OpenID Connect provider <https://gams.com/engine/administration.html#oidc-identity-providers>`__, and all users who wish to log into MIRO Server must also use this identity provider. Note that the *Client ID* you specified in the MIRO Server configuration must be added to the list of additional client IDs (*extra_client_ids*) of GAMS Engine.

.. admonition:: Note
   :class: warning

   In your OpenID Connect provider, you need to register the following redirect URI: *http(s)://www.yourmiroserver.com/login/oauth2/code/miroserver*.

.. _server-auth-oidc-logout:

Logging out
~~~~~~~~~~~

When a user clicks *Sign Out*, MIRO Server ends its own session and then redirects the user to the *proxy.openid.logout-url*. If no logout URL is set, the session at the identity provider is not ended. The next time the user clicks *Login*, the identity provider sees the active session and signs the user back in without asking for credentials, so it looks as if logging out had no effect. To end the session at the identity provider as well, set *proxy.openid.logout-url* to the logout endpoint of your identity provider. You can find this endpoint as ``end_session_endpoint`` in the provider's discovery document (*https://<your-identity-provider>/.well-known/openid-configuration*).

To send users back to MIRO Server after logging out, add the URL of the MIRO Server page *http(s)://www.yourmiroserver.com/logout-success* to the logout URL, URL-encoded (typically as the ``post_logout_redirect_uri`` parameter). Most identity providers only redirect to URLs that you have registered with them, e.g. as *Front-channel logout URL* in Microsoft Entra ID or as *Valid post logout redirect URIs* in Keycloak.

Some identity providers also require the user's ID token to be passed in the ``id_token_hint`` parameter. You can insert it into the logout URL with the expression ``#{oidcUser.idToken.tokenValue}``. An example for Keycloak:

.. code:: text

   proxy:
     [...]
     authentication: openid
     openid:
       [...]
       logout-url: https://keycloak.example.com/realms/<your-realm>/protocol/openid-connect/logout?id_token_hint=#{oidcUser.idToken.tokenValue}&post_logout_redirect_uri=https%3A%2F%2Fwww.yourmiroserver.com%2Flogout-success

.. admonition:: Note

   Google does not provide a logout endpoint for OpenID Connect. The Google logout URL shown above signs the user out of all their Google accounts in the browser, not just MIRO Server.



.. _extending-the-miro-docker-image:

.. _server-extend:

Extending the MIRO Docker image
-------------------------------



If your MIRO applications need additional R packages, you must extend the `MIRO UI Docker image <https://github.com/GAMS-dev/miro/tree/master/server/ui/Dockerfile>`__. You can do so by adding the additional packages required by your custom renderers to the file ``additional_packages`` located inside the `miro_server <#miro_server>`__ directory. Each package name must be on a new line. Once all packages are added to this file, run

.. code-block:: bash

   > ./miro-server build

Please note that additional packages may cause version conflicts with packages used internally by MIRO. We therefore recommend keeping the number of additional packages to a minimum.


.. _nginx-example-configuration:

.. _server-nginx:

Nginx example configuration
---------------------------



We recommend running GAMS MIRO Server behind a reverse proxy such as `Nginx <https://www.nginx.com/>`__. You can find an example configuration in the file ``miro.conf`` located inside the *miro_server* directory mentioned in the installation instructions. To apply this configuration, copy it to the configuration location of your Nginx (e.g. ``sudo cp miro.conf /etc/nginx/conf.d/``). Afterwards, reload Nginx using ``sudo nginx -s reload``.

The ``miro.conf`` file only forwards requests to the MIRO Server web interface. To also make the `MIRO Server REST API <./server_api.html>`__ accessible via ``/api``, add a ``location /api/`` block that forwards requests to port ``8081``, as shown in the `example configuration <#nginx-example>`__ below.

.. raw:: html

   <span id="server-engine-on-same-host"></span>

**Hosting GAMS Engine and MIRO Server on the same host**
When you host GAMS Engine and MIRO Server on the same host, you have to merge the Nginx configurations of both GAMS Engine and MIRO Server. A resulting ``/etc/nginx/conf.d/miro.conf`` could look as follows:

.. dropdown:: Nginx example configuration
   :name: nginx-example

   ::

      map $http_upgrade $connection_upgrade {
         default upgrade;
         ''      close;
      }
      # redirect all http to https (optional)
      #server {
      #    listen 80;
      #    return 301 https://$host$request_uri;
      #}
      server {
          listen 80;
      # SSL setup (optional): replace "listen 80" above with
      #    listen 443 ssl;
      #    ssl_certificate /path/to/certificate/file.pem;
      #    ssl_certificate_key /path/to/key/file.key.pem;
          location / {
              proxy_pass http://127.0.0.1:8080;

              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection $connection_upgrade;
              proxy_read_timeout 600s;

              proxy_redirect    off;
              proxy_set_header  Host             $http_host;
              proxy_set_header  X-Real-IP        $remote_addr;
              proxy_set_header  X-Forwarded-For  $proxy_add_x_forwarded_for;
              proxy_set_header  X-Forwarded-Proto $scheme;
          }
          location /api/ {
              proxy_pass http://127.0.0.1:8081;

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

Note that even though both GAMS Engine and MIRO Server run on the same host, the GAMS Engine host is not ``localhost``. If you use TLS (certificates) or custom ports, add these settings here as well. If you need help with this, please contact us. Nginx needs to be reloaded if the configuration has been changed: ``sudo nginx -s reload``.

If you want to host MIRO Server on a path other than root, you must change the context path in the file *data/application.yml* accordingly (entry *server.servlet.context-path*).


.. _running-under-selinux:

.. _selinux:

Running under SELinux
---------------------



Note that with SELinux active (e.g. CentOS/RHEL), you might have to allow your nginx server to proxy to the upstream MIRO Server host. You can do so by running: ``setsebool -P httpd_can_network_connect 1``.


.. _example-install-miro-server-and-gams-engine-on-the-same-machine:

.. _same-machine-example:

Example: Install MIRO Server and GAMS Engine on the same machine
-----------------------------------------------------------------



If the `Engine One <https://gams.com/sales/engine_facts/>`__ instance is running on the same machine as MIRO Server, there are a few things to consider. The following example shows how to install MIRO Server and GAMS Engine on a freshly installed Ubuntu 24.04 server.

.. _planned-configuration:

.. rubric:: Planned configuration


- Access MIRO apps under https://my-miro.com/
- Access GAMS Engine under https://my-miro.com/engine/


.. _prerequisites:

.. rubric:: 1) Prerequisites


#. Install and start `Docker <https://docs.docker.com/engine/install/>`__.
#. We recommend configuring Docker to `start on boot <https://docs.docker.com/engine/install/linux-postinstall/#configure-docker-to-start-on-boot>`__ and to `manage Docker as a non-root user <https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user>`__.
#. Install a reverse proxy, e.g. `Nginx <https://www.nginx.com/resources/wiki/start/topics/tutorials/install/>`__.
#. To install MIRO Server and GAMS Engine, you need a tool to unzip archives, e.g. *unzip*: ``sudo apt install unzip``.


.. _install-gams-engine:

.. rubric:: 2) Install GAMS Engine


Since the MIRO Server setup requires the GAMS Engine location to be specified, it must be installed first.

#. Download the `GAMS Engine zip file <https://gams.com/engine/installation.html>`__ and place it, e.g., in the home directory ``/home/ubuntu``.
#. Unzip the archive and ``cd`` into the ``gams_engine`` directory.
#. Run the script ``./up.sh``, e.g. ``./up.sh -a test1234 -m engine -p 8095 -w 4``.

   - ``-a test1234``: Default password for Engine admin user.
   - ``-m engine``: By default, GAMS Engine is mounted under ``/``. Since we want to access it under https://my-miro.com/engine/, we need to specify this here.
   - ``-p 8095``: The Engine port, which we will use in the Nginx configuration. Since MIRO Server runs on the same machine, we must avoid port 8081, which is reserved by MIRO Server.
   - ``-w 4``: 4 GAMS workers for solving incoming jobs.
   - Note that no certificate files were specified in this step. These will be configured in the Nginx configuration later.


.. _install-gams-miro-server:

.. rubric:: 3) Install GAMS MIRO Server


#. Download the `MIRO Server zip file <./download.html#miro-server>`__ and place it, e.g., in the home directory ``/home/ubuntu``.
#. Unzip the archive and ``cd`` into the ``miro_server`` directory.
#. Get the IP of the ``docker0`` network interface: ``sudo ip addr show docker0 |grep -Po 'inet \K[\d.]+'``.
#. Run ``./miro-server install``.

   - Host: ``http://172.17.0.1:8095/engine/api``.
   - Namespace: ``global``. This is the default Engine namespace. If GAMS Engine is to be used by clients other than MIRO Server, we recommend using a different namespace.
   - Use MIRO Server with authentication service: ``Yes``.

#. Run ``./miro-server start``.


.. _configure-nginx:

.. rubric:: 4) Configure Nginx


We configure Nginx to forward the root URL https://my-miro.com/ to MIRO Server and https://my-miro.com/engine to GAMS Engine. Where and how the configuration is stored depends on the Linux distribution. In this example, we use Ubuntu 24.04.

#. Add an Nginx configuration file, e.g. ``miro.conf`` under ``/etc/nginx/sites-available``. We use the `Nginx example configuration <#nginx-example>`__ as a starting point.

   - Specify certificate files. In the example configuration these are commented out.
   - Under ``location /engine`` we specify the Engine port by replacing ``proxy_pass http://127.0.0.1:5000`` with ``proxy_pass http://127.0.0.1:8095``.

#. Ubuntu-only: Run ``sudo ln -sf /etc/nginx/sites-available/miro.conf /etc/nginx/sites-enabled/default`` to replace the default configuration.
#. Run ``sudo nginx -s reload``.


.. _example-install-multiple-miro-server-instances-on-the-same-machine:

.. _multiple-server-example:

Example: Install multiple MIRO Server instances on the same machine
-------------------------------------------------------------------



Besides running GAMS Engine and MIRO Server on the same machine, you can also run multiple MIRO Server instances there. This is useful, for example, if some MIRO applications should be showcased, while others should only be accessible to registered users. The following example builds on the `previous configuration <#same-machine-example>`__ and adds another MIRO Server instance, which will be accessible under https://my-miro.com/server_2/.

.. _install-a-second-miro-server-instance:

.. rubric:: Install a second MIRO Server instance


#. Under ``/home/ubuntu`` create a directory, e.g. ``miro_server_2``.

#. Unzip the `MIRO Server archive <./download.html#miro-server>`__ into that new folder. Make sure that the files are directly located in ``miro_server_2`` (e.g. ``/home/ubuntu/miro_server_2/docker-compose.yml``) and not further nested in a ``miro_server`` folder.

#. ``cd`` into ``miro_server_2``.

#. In the ``docker-compose.yml`` file, the default ports used by some MIRO services need to be changed to avoid conflicts with the first MIRO Server instance.

   - Change ``services.auth.ports`` from ``8081:1234`` to e.g. ``8096:1234``
   - Change ``services.proxy.ports`` from ``8080:8080`` to e.g. ``8090:8090``
   - Add ``SCRIPT_NAME: /server_2`` under ``services.auth.environment``, so that the REST API is served under ``/server_2/api``

#. In ``data_raw/application.yml``:

   - Change ``proxy.port`` from ``8080`` to ``8090`` (same port as in ``services.proxy.ports`` in the ``docker-compose.yml``)
   - Change ``server.servlet.context-path`` from ``/`` to ``/server_2``. The ``context-path`` specifies under which URL MIRO Server should be accessible. By specifying ``/server_2`` the second MIRO Server instance will be accessible under https://my-miro.com/server_2/.

#. Run ``./miro-server install``

   - Host: ``http://172.17.0.1:8095/engine/api``.
   - Namespace: ``server_2``. We use a different namespace for each MIRO Server instance. This namespace must be `created in Engine <https://gams.com/engine/administration.html#namespaces>`__ beforehand.
   - Use MIRO Server with authentication service: ``Yes``.

#. In the Nginx configuration we now specify the location of the second MIRO Server instance, including its REST API.

   .. rubric:: Nginx configuration to add
      :name: nginx-two-server


   .. container::

      ::

             location /server_2 {
                 proxy_pass http://127.0.0.1:8090;

                 proxy_http_version 1.1;
                 proxy_set_header Upgrade $http_upgrade;
                 proxy_set_header Connection $connection_upgrade;
                 proxy_read_timeout 600s;

                 proxy_redirect    off;
                 proxy_set_header  Host             $http_host;
                 proxy_set_header  X-Real-IP        $remote_addr;
                 proxy_set_header  X-Forwarded-For  $proxy_add_x_forwarded_for;
                 proxy_set_header  X-Forwarded-Proto $scheme;
             }
             location /server_2/api/ {
                 proxy_pass http://127.0.0.1:8096;

                 proxy_redirect    off;
                 proxy_set_header  Host             $http_host;
                 proxy_set_header  X-Real-IP        $remote_addr;
                 proxy_set_header  X-Forwarded-For  $proxy_add_x_forwarded_for;
                 proxy_set_header  X-Forwarded-Proto $scheme;
             }

   Requests for ``/server_2`` are passed to port ``8090`` which we also specified in the ``docker-compose.yml`` and the ``data_raw/application.yml`` file. The MIRO REST API can be accessed via ``/server_2/api``. The port corresponds to the one used in ``docker-compose.yml``.

#. Reload Nginx via ``sudo nginx -s reload``.
