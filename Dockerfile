## largely based on rocker r-base image (GPL >= 2.0)

FROM ubuntu:xenial

LABEL com.gamsmiro.vendor="GAMS Development Corp."\
com.gamsmiro.version="1.1.99"\
com.gamsmiro.license="GPL-3.0"\
com.gamsmiro.description="GAMS MIRO Docker image"\
maintainer="rschuchmann@gams.com"

ARG R_BASE_VERSION=4.0.2
ARG BUILD_DOCKER=true
ARG SCRIPTS_PATH=/home/miro/scripts


RUN groupadd -g 1000 miro && \
    useradd -r -u 1000 -g miro miro

ENV APP /home/miro/app
RUN mkdir -p $APP
RUN chown -R miro /home/miro

RUN apt-get update \ 
    && apt-get upgrade -y \
	&& apt-get install -y --no-install-recommends \
		ed \
		less \
		locales \
		vim-tiny \
		wget \
		ca-certificates \
		apt-transport-https \
		gsfonts \
	&& rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran40/" > /etc/apt/sources.list.d/cran.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9


## Now install R 
## Also set a default CRAN repo
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		r-base=${R_BASE_VERSION}* \
		r-base-dev=${R_BASE_VERSION}* \
		r-recommended=${R_BASE_VERSION}* \
        && echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/*


########

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    software-properties-common

RUN add-apt-repository ppa:deadsnakes/ppa

RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libv8-dev \
    libpq-dev \
    python3.8 \
    python3-pip

RUN pip3 install --upgrade pip
RUN pip3 install numpy==1.14.5 pandas==0.23.1 matplotlib==2.2.2

# install custom packages
RUN mkdir /home/miro/r
COPY r/library_src /home/miro/r/library_src
COPY scripts /home/miro/scripts

RUN Rscript ./home/miro/scripts/install_source.R

RUN rm -rf /home/miro/r /home/miro/scripts

# copy MIRO to the image
COPY src /home/miro/app/

USER miro

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/miro/app', port = 3838, host = '0.0.0.0')"]
