## largely based on rocker r-ubuntu image (GPL >= 2.0)

FROM ubuntu:24.04 AS miro-base

ARG R_BASE_VERSION=4.2.0
## we need to set this environment variable so it can be used when extending the
## base image (e.g. r-base-dev=${R_BASE_VERSION}*)
ENV R_BASE_VERSION=${R_BASE_VERSION}

RUN groupadd -g 1001 miro && \
    useradd -r -u 1001 -g miro miro

ENV APP /home/miro/app
RUN mkdir -p $APP
RUN chown -R miro /home/miro

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    software-properties-common \
    dirmngr \
    locales \
    ca-certificates \
    wget \
    gpg-agent \
    && wget -q -O - https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
    | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
    && add-apt-repository --enable-source --yes "ppa:marutter/rrutter4.0"

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive
ENV TZ UTC


## Now install R
## Also set a default CRAN repo
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    r-base=${R_BASE_VERSION}* \
    r-recommended=${R_BASE_VERSION}* \
    && echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir /home/miro/r




FROM miro-base AS miro-pkg-builder

ARG R_BASE_VERSION=4.2.0
ARG BUILD_DOCKER=true
ARG SCRIPTS_PATH=/home/miro/scripts

RUN apt-get update && apt-get install -y \
    r-base-dev=${R_BASE_VERSION}*

ARG OPENSSL_VERSION=3.0.3

# install openssl3 from source
RUN wget https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz && \
    tar -xvzf openssl-${OPENSSL_VERSION}.tar.gz && \
    cd openssl-${OPENSSL_VERSION} && \
    ./config no-shared && \
    make && \
    make install

# make sure the openssl R package finds the libraries
ARG OPENSSL_PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig

ARG CURL_VERSION=7.83.1

# install curl from source
RUN wget https://curl.se/download/curl-${CURL_VERSION}.tar.gz && \
    tar -xvzf curl-${CURL_VERSION}.tar.gz && \
    cd curl-${CURL_VERSION} && \
    env PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig ./configure --with-openssl --without-libpsl --disable-shared && \
    make && \
    make install

ARG PQ_VERSION=14.3

# install libpq from source
RUN wget https://ftp.postgresql.org/pub/source/v${PQ_VERSION}/postgresql-${PQ_VERSION}.tar.gz && \
    tar -xvzf postgresql-${PQ_VERSION}.tar.gz && \
    cd postgresql-${PQ_VERSION} && \
    ./configure --with-openssl --with-includes=/usr/local/lib64/include --with-libraries=/usr/local/lib64 CFLAGS='-Wl,-rpath=/usr/local/lib64' && \
    make && \
    make -C src/bin install && \
    make -C src/include install && \
    make -C src/interfaces install && \
    make -C doc install

ARG LD_LIBRARY_PATH=/usr/local/pgsql/lib

# make sure the RPostgres R package finds the libraries
ARG PKG_CONFIG_PATH="/usr/local/pgsql/lib/pkgconfig:${OPENSSL_PKG_CONFIG_PATH}"

# install custom R packages
COPY r/library_src /home/miro/r/library_src
COPY scripts /home/miro/scripts

RUN Rscript ./home/miro/scripts/install_source.R


# install custom packages
COPY r/library_src /home/miro/r/library_src
COPY scripts /home/miro/scripts

RUN Rscript ./home/miro/scripts/install_source.R




FROM miro-base

LABEL com.gamsmiro.vendor="GAMS Development Corp."\
    com.gamsmiro.version="2.10.9999"\
    com.gamsmiro.license="GPL-3.0"\
    com.gamsmiro.description="GAMS MIRO Docker image"\
    maintainer="fproske@gams.com"

# copy R packages from miro-pkg-builder
COPY --from=miro-pkg-builder /usr/local/pgsql/lib/libpq.so.5 /usr/local/lib/libpq.so.5
COPY --from=miro-pkg-builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

ENV LD_LIBRARY_PATH=/usr/local/lib

# copy MIRO to the image
COPY src /home/miro/app/

RUN apt-get update \
    && apt-get upgrade -y

USER miro

EXPOSE 3838

WORKDIR /home/miro/app/

CMD ["R", "-e", "shiny::runApp('.', port = 3838, host = '0.0.0.0')"]
