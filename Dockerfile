## largely based on rocker r-base image

FROM ubuntu:xenial

LABEL com.gamsmiro.vendor="GAMS Development Corp."\
com.gamsmiro.version="0.7.1"\
com.gamsmiro.license="GPL-2.0"\
com.gamsmiro.description="GAMS MIRO Docker image"\
maintainer="rschuchmann@gams.com"

ARG R_BASE_VERSION=3.6.0

## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
RUN useradd docker \
	&& mkdir /home/docker \
	&& chown docker:docker /home/docker \
	&& addgroup docker staff

RUN apt-get update \ 
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

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran35/" > /etc/apt/sources.list.d/cran.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9


## Now install R and littler, and create a link for littler in /usr/local/bin
## Also set a default CRAN repo, and make sure littler knows about it too
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		littler\
                r-cran-littler \
		r-base=${R_BASE_VERSION}* \
		r-base-dev=${R_BASE_VERSION}* \
		r-recommended=${R_BASE_VERSION}* \
        && echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
        && echo 'source("/etc/R/Rprofile.site")' >> /etc/littler.r \
	&& ln -s /usr/share/doc/littler/examples/install.r /usr/local/bin/install.r \
	&& ln -s /usr/share/doc/littler/examples/install2.r /usr/local/bin/install2.r \
	&& ln -s /usr/share/doc/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
	&& ln -s /usr/share/doc/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
	&& install.r docopt \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/*


########

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    software-properties-common python-software-properties 

RUN apt-get update && apt-get install -y \
    sudo \
    libcurl-dev \
    libcairo2-dev \
    libv8-3.14-dev \
    unixodbc\
    unixodbc-dev\
    odbc-postgresql
# basic shiny functionality
RUN R -e "install.packages('shiny', repos='http://cran.us.r-project.org/')"

# install dependencies of the app
RUN R -e "install.packages(c('R6', 'stringi', 'shinydashboard', 'processx', 'V8', 'dplyr', 'readr', 'readxl', 'writexl', 'rhandsontable', 'jsonlite', 'jsonvalidate', 'rpivotTable', 'futile.logger', 'zip', 'tidyr', 'odbc', 'DBI', 'digest', 'DT', 'leaflet', 'leaflet.minicharts', 'RColorBrewer', 'plotly', 'dygraphs', 'xts', 'httr', 'future'), repos='http://cran.us.r-project.org/')"


# copy cmex, opt and err file; r-x permissions for cmex file
COPY gams_data ${R_GAMS_SYSDIR}
RUN chmod 755 ${R_GAMS_SYSDIR}/gamscmex.out

#install pip and some python packages
RUN sudo apt-get -y install python3-pip
RUN pip3 install numpy pandas matplotlib geocoder geopy

# environment variable for GDX API
ENV R_GAMS_SYSDIR="/home/docker/gams"
RUN mkdir /home/docker/gams

#install GDXRRW packagpe
COPY gams_data /home/docker/gams
RUN R -e "install.packages('/home/docker/gams/gdxrrw_1.0.4.999.tar.gz', repos = NULL, type = 'source')"
RUN rm /home/docker/gams/gdxrrw_1.0.4.999.tar.gz

# copy MIRO to the image
RUN mkdir /home/docker/miro
COPY miro_data /home/docker/miro

WORKDIR /
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/docker/miro')"]

