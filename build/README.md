# Build MIRO from Source

## Build dependencies
In order to build MIRO from source, you need to first install the necessary build dependencies. These are:

* Nodejs (>= 13.2.0)
* yarn (>= 1.19.1)
Note that both node and yarn need to be added to the [PATH](https://en.wikipedia.org/wiki/PATH_%28variable%29).

### MacOS
* R (>=3.6.1)

In order to compile the data.table package from source, you need an OpenMP compatible C compiler. See [here](https://github.com/Rdatatable/data.table/wiki/Installation#openmp-enabled-compiler-for-mac) for more information.
Additionally, v8 needs to be installed. To install it via [Homebrew](https://brew.sh), use: `brew update && brew install v8`.

On some systems, libpng is also required. To install via [Homebrew](https://brew.sh), use: `brew update && brew install libpng`.

### Windows
You will need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) to compile R packages from source.
To extract the R binaries from the Inno installer file, you need [innoextract](https://constexpr.org/innoextract/). Make sure it is added to your PATH

Note: If you want to build on a CI server, it is recommended to install R (>=3.6.1) as well. This saves time as the R development libraries will not need to be reinstalled with every build.

To build the gdxrrwMIRO package, you need either an intel compiler (https://software.intel.com/en-us/compilers) or Visual Studio Express (https://visualstudio.microsoft.com/vs/express/). For the Visual Studio Express 2019 Community Edition, the component 'Desktop development with C++' has to be installed via Visual Studio installer. Also make sure that the gdxlib path in src/Makevars matches your folder structure, e.g. "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat". Also control the path in the call in src/build-win.bat, e.g. "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat".

### Linux
* R (>=3.6.1)

Additionally, the following system libraries are required:

* libcurl
    - deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
    - rpm: libcurl-devel (Fedora, CentOS, RHEL)
    - csw: libcurl_dev (Solaris)
* libxml
    - deb: libxml2-dev (Debian, Ubuntu, etc)
    - rpm: libxml2-devel (Fedora, CentOS, RHEL)
    - csw: libxml2_dev (Solaris)
* libv8
    - deb: libv8-dev or libnode-dev (Debian / Ubuntu)
    - rpm: v8-devel (Fedora, EPEL)
    - brew: v8 (OSX)
    - csw: libv8_dev (Solaris)

### Docker
* Docker
* R (>=3.6.1)

Additionally, the following system library is required:

* odbc
    - deb: unixodbc-dev (Debian, Ubuntu, etc)
    - rpm: unixODBC-devel (Fedora, CentOS, RHEL)
    - csw: unixodbc_dev (Solaris)
    - brew: unixodbc (Mac OSX)


## Building
1. Fetch the github repo (`git clone --recurse-submodules <repo_url>`). Note that in order for images to be pulled correctly, git lfs needs to be installed.
1. Install node packages: `yarn install`
1. Build: `yarn dist`


## Signing
Note that in order for your artifact to be properly signed, you need to specify the credentials via environment variables:
### Windows
* `CSC_LINK` needs to be set to the cert file (\*.p12/\*.pfx). See [here](https://www.electron.build/code-signing) for more information.
* `CSC_KEY_PASSWORD`: The password to decrypt the certificate given in `CSC_LINK`

### MacOS
* Codesign identity: `CODESIGN_IDENTITY`
* Apple ID (for notarisation): `APPLEID`
* Apple ID password (for notarisation): `APPLEIDPASS`
