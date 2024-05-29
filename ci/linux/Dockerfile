FROM node:20-bookworm

ARG R_BASE_VERSION=4.4.0
ENV R_BASE_VERSION=${R_BASE_VERSION}
ARG OPENSSL_VERSION=3.1.5
ENV OPENSSL_VERSION=${OPENSSL_VERSION}

ARG GAMS_MAJOR=46
ARG GAMS_MINOR=5
ARG GAMS_MAINT=0

# R installation mostly taken from r base image (https://github.com/rocker-org/rocker) (GPL >= 2)

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        ed \
        less \
        locales \
        vim-tiny \
        wget \
        ca-certificates \
        fonts-texgyre \
        openssh-client \
        rsync \
        libxml2-dev \
        libfribidi-dev \
        libharfbuzz-dev \
        && rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
        && locale-gen en_US.utf8 \
        && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

## Use Debian unstable via pinning -- new style via APT::Default-Release
RUN echo "deb http://cloud.r-project.org/bin/linux/debian bullseye-cran40/" >> /etc/apt/sources.list

## During the freeze, new (source) packages are in experimental and we place the binaries in our PPA
#RUN echo "deb http://deb.debian.org/debian experimental main" > /etc/apt/sources.list.d/experimental.list \
#    && echo "deb [trusted=yes] https://eddelbuettel.github.io/ppaR400 ./" > /etc/apt/sources.list.d/edd-r4.list

## Now install R and littler, and create a link for littler in /usr/local/bin
RUN gpg --keyserver keyserver.ubuntu.com --recv-key '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7' \
        && gpg --armor --export '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7' | tee /etc/apt/trusted.gpg.d/cran_debian_key.asc \
        && apt-get update \
        && apt-get install -y --no-install-recommends \
        libopenblas0-pthread \
        littler \
        r-cran-docopt \
        r-cran-littler \
        r-base=${R_BASE_VERSION}-* \
        r-base-dev=${R_BASE_VERSION}-* \
        r-base-core=${R_BASE_VERSION}-* \
        r-recommended=${R_BASE_VERSION}-* \
        && ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
        && ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
        && ln -s /usr/lib/R/site-library/littler/examples/installBioc.r /usr/local/bin/installBioc.r \
        && ln -s /usr/lib/R/site-library/littler/examples/installDeps.r /usr/local/bin/installDeps.r \
        && ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
        && ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
        && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
        && rm -rf /var/lib/apt/lists/*

RUN wget https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_MAJOR}.${GAMS_MINOR}.${GAMS_MAINT}/linux/linux_x64_64_sfx.exe && \
        chmod a+x linux_x64_64_sfx.exe && \
        ./linux_x64_64_sfx.exe && \
        rm linux_x64_64_sfx.exe && \
        mv "gams${GAMS_MAJOR}.${GAMS_MINOR}_linux_x64_64_sfx" "/opt/gams"

ENV GAMS_SYS_DIR=/opt/gams

RUN R -e "install.packages(c('xml2', 'precommit', 'docopt', 'styler'));"

# install openssl3 from source
RUN wget https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz && \
        tar -xvzf openssl-${OPENSSL_VERSION}.tar.gz && \
        cd openssl-${OPENSSL_VERSION} && \
        ./config no-shared && \
        make && \
        make install

# make sure the openssl R package finds the libraries
ENV OPENSSL_PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        gnupg \
        lsb-release \
        python3-pip

RUN mkdir -p /etc/apt/keyrings \
        && curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg \
        && echo \
        "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian \
        $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null

RUN  apt-get update \
        && apt-get install -y --no-install-recommends \
        docker-ce \
        docker-ce-cli \
        containerd.io \
        docker-compose-plugin

# required for uploading files to s3 bucket
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
        && unzip awscliv2.zip \
        && ./aws/install \
        && rm -rf aws awscliv2
