Download & installation
=======================

.. _installation-section:

Install GAMS MIRO Desktop
-------------------------

.. tab-set::

   .. tab-item:: GAMS

      .. raw:: html

         <span id="gams-1"></span>

      .. note::

         To solve models in GAMS MIRO, you need GAMS 30.2 or later. You can download the latest GAMS version `here <https://www.gams.com/download/>`__.

      GAMS MIRO Desktop itself is open source software. For GAMS to interact with MIRO, the ``MIRO connector`` is required, which is included in the GAMS system `from GAMS version 30.2 <https://www.gams.com/download>`__. Without a valid license, the connector has limitations on the number of indexed symbols that can be communicated with MIRO. A maximum of 10 indexed symbols can be included in `$on/offExternalInput and $on/offExternalOutput <./model.html#model-adjustments>`__.

   .. tab-item:: GAMSPy

      .. raw:: html

         <span id="gamspy-1"></span>

      .. note::

         To solve models in GAMS MIRO, you need GAMSPy 0.12 or later.

      Both GAMS MIRO Desktop and GAMSPy are open source software.

Please check the `release notes <./release.html>`__.

.. tab-set::

   .. tab-item:: Windows (x86_64)

      .. _windows:

      .. button-link:: https://d37drm4t2jghv5.cloudfront.net/miro/__VERSION__/windows/GAMS-MIRO-Setup-__VERSION_FULL__.exe
               :color: primary

               Download for Windows

      SHA-256 hash: ``__SHA_HASH_WIN__``

      By downloading this software, you agree to the `License Agreement <./license.html>`__.

      .. rubric:: Installation


      To install GAMS MIRO and its components, download the latest version and run the setup, which will guide you through the installation process. Make sure that no MIRO application is running during this time.

   .. tab-item:: macOS (x86_64/arm64)

      .. _macos:

      .. button-link:: https://d37drm4t2jghv5.cloudfront.net/miro/__VERSION__/macosxarm/GAMS-MIRO-__VERSION_FULL__.dmg
               :color: primary

               Download for macOS (Apple silicon)

      SHA-256 hash: ``__SHA_HASH_MAC_ARM__``

      .. button-link:: https://d37drm4t2jghv5.cloudfront.net/miro/__VERSION__/macosx/GAMS-MIRO-__VERSION_FULL__.dmg
               :color: primary

               Download for macOS (x86_64)

      SHA-256 hash: ``__SHA_HASH_MAC_X86__``

      By downloading this software, you agree to the `License Agreement <./license.html>`__.

      .. rubric:: Installation


      To install GAMS MIRO and its components, download the latest version.

   .. tab-item:: Linux (x86_64)

      .. _linux:

      .. tab-set::

         .. tab-item:: Ubuntu/Debian

            .. _install-linux-deb:

            .. button-link:: https://d37drm4t2jghv5.cloudfront.net/miro/__VERSION__/linux/GAMS-MIRO-__VERSION_FULL__.deb
               :color: primary

               Download for Debian/Ubuntu (deb)

            SHA-256 hash: ``__SHA_HASH_DEB_LINUX__``

            By downloading this software, you agree to the `License Agreement <./license.html>`__.

            .. rubric:: Installation


            #. The first time you launch GAMS MIRO, some R packages need to be installed. They are installed in your home directory (``$XDG_CONFIG_HOME/miro-library`` or ``~/.config/miro-library``). You will be guided through this installation process.

         .. tab-item:: Other

            .. _install-linux-appimage:

            .. button-link:: https://d37drm4t2jghv5.cloudfront.net/miro/__VERSION__/linux/GAMS-MIRO-__VERSION_FULL__.AppImage
               :color: primary

               Download for Linux (AppImage)

            SHA-256 hash: ``__SHA_HASH_LINUX__``

            By downloading this software, you agree to the `License Agreement <./license.html>`__.

            .. rubric:: Installation


            #. If a current R version (4.4 or later) is not yet installed on your system, install it first. You can download R `here <https://cloud.r-project.org/>`__.
            #. For MIRO to work, compilers as well as the system libraries libpng and libcurl need to be installed. For example, on Fedora:

               .. code-block:: console

                  > sudo dnf install libcurl-devel libpng-devel

               If no compilers are installed on your system yet, we recommend installing the build tools: ``> sudo dnf groupinstall "Development Tools"`` (Fedora).
            #. The first time you launch GAMS MIRO, some R packages need to be installed. They are installed either in the R installation path (``rPath/miro-library``) or in the user's home directory (``$XDG_CONFIG_HOME/miro-library`` or ``~/.config/miro-library``) depending on whether MIRO was installed for all users or only for the current user. You will be guided through this installation process.

            .. note::

               For a complete system integration of the MIRO AppImage (e.g. file associations and custom URL schemes), please install the AppImage via the `AppImageLauncher <https://github.com/TheAssassin/AppImageLauncher>`__.

.. _miro-workspace:

GAMS MIRO workspace
~~~~~~~~~~~~~~~~~~~

The workspace is where `deployed apps <./deployment.html>`__ are stored and cached and where logs are written. The MIRO database with all user scenarios is also located there. Furthermore, all user settings and `additional R packages <./configuration_advanced.html#additional-r-packages>`__ are kept in the workspace.

On all platforms, the MIRO workspace is located at ``~/.miro``. On macOS and Linux, ``~`` resolves to ``$HOME``; on Windows, it resolves to ``%USERPROFILE%``.

.. _uninstallation-section:

Uninstall GAMS MIRO
-------------------

To uninstall the software, follow the default procedure of your operating system. The uninstaller only deletes the data in the directory where GAMS MIRO was installed. On Windows, you will be asked if you also want to permanently delete the MIRO database with all your settings. This refers to data in the `MIRO workspace <#miro-workspace>`__. If you agree, the entire directory will be deleted. On macOS and Linux, the MIRO workspace is not deleted. To remove this data permanently, delete the workspace manually.

.. _demo-section:

Demo applications
-----------------

We provide several GAMS MIRO demo applications that introduce MIRO's basic features. To run the demo applications, you must first extract them. Learn more `here <./getting_started.html#demo-applications>`__.

.. container:: miro-download-buttons

   .. button-link:: ./GAMS-MIRO-Demo-Apps.zip
      :color: primary

      Download GAMS demo apps

   .. button-link:: ./GAMS-MIRO-Demo-Apps-GAMSPy.zip
      :color: primary

      Download GAMSPy demo apps

.. raw:: html

   <span id="miro-server"></span>

GAMS MIRO Server
----------------

.. note::

   Currently, GAMS MIRO Server is only available for MIRO applications based on GAMS models, not for GAMSPy models.

.. grid:: 1 1 2 2
   :gutter: 2

   .. grid-item::

      Like GAMS MIRO Desktop, `GAMS MIRO Server <./server.html>`__ is open source software. However, the setup requires the use of `GAMS Engine <https://www.gams.com/sales/engine_facts/>`__ for the model calculations, which must be purchased separately.

      Detailed installation instructions and further information on administrative aspects can be found `here <./server.html>`__.

      .. button-link:: ./GAMS-MIRO-Server-__VERSION_FULL__.zip
         :color: primary

         Download GAMS MIRO Server

   .. grid-item::

      .. image:: ../assets/images/server/miro-server.png
         :alt: GAMS MIRO Server
         :class: miro-image-max-height-350pt
         :align: center
