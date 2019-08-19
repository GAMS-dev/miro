## largely based on rocker r-base image

FROM ubuntu:xenial

LABEL com.gamsmiro.vendor="GAMS Development Corp."\
com.gamsmiro.version="0.7.1"\
com.gamsmiro.license="GPL-3.0"\
com.gamsmiro.description="GAMS MIRO Docker image"\
maintainer="rschuchmann@gams.com"

ARG R_BASE_VERSION=3.6.0


RUN groupadd -g 999 miro && \
    useradd -r -u 999 -g miro miro

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

RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran35/" > /etc/apt/sources.list.d/cran.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9


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
    libv8-3.14-dev \
    unixodbc \
    unixodbc-dev \
    odbc-postgresql \
    python3.6 \
    python3-pip

RUN pip3 install --upgrade pip
RUN pip3 install numpy pandas matplotlib geocoder geopy

# install custom packages

# first install dependencies of custom packages
RUN R -e "install.packages(c('assertthat', 'crayon', 'cli', 'fansi', 'utf8', 'Rcpp', 'R6', 'BH', 'magrittr', 'httpuv', 'mime', 'jsonlite', 'digest', 'sourcetools', 'later', 'promises', 'rlang', 'xtable', 'fastmap'), repos='http://cran.us.r-project.org/')"
RUN mkdir /home/miro/packages
COPY packages /home/miro/packages

# basic shiny functionality
RUN R -e "install.packages('/home/miro/packages/htmltools_0.3.6.9004.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/shiny_1.3.2.9999.tar.gz', repos = NULL, type = 'source')"



RUN R -e "install.packages(c('colorspace', 'purrr', 'yaml', 'labeling', 'munsell', 'lazyeval', 'glue', 'pkgconfig', 'tidyselect', 'plogr', 'htmlwidgets', 'base64enc', 'png', 'RColorBrewer', 'raster', 'scales', 'sp', 'viridisLite', 'zeallot', 'ellipsis'), repos='http://cran.us.r-project.org/')"

RUN R -e "install.packages('/home/miro/packages/crosstalk_1.0.999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/DT_0.7.2.9999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/gdxrrw_1.0.4.999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/leaflet_2.0.2.999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/vctrs_0.2.0.9999.tar.gz', repos = NULL, type = 'source')"

RUN R -e "install.packages(c('pillar', 'tibble', 'dplyr', 'sys', 'askpass', 'prettyunits', 'stringi', 'curl', 'DBI', 'blob', 'hms', 'tidyr', 'data.table'), repos='http://cran.us.r-project.org/')"

RUN R -e "install.packages('/home/miro/packages/httr_1.4.0.9999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/odbc_1.1.6.999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/plotly_4.9.999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/shinydashboard_0.7.1.999.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('/home/miro/packages/timevis_0.5.0.9999.tar.gz', repos = NULL, type = 'source')"
RUN rm -rf /home/miro/packages

# install dependencies of the app
RUN R -e "install.packages(c('rematch', 'formatR', 'ps', 'clipr', 'cellranger', 'progress', 'lambda.r', 'futile.options', 'zoo', 'globals', 'listenv', 'processx', 'V8', 'readr', 'readxl', 'writexl', 'rhandsontable', 'jsonvalidate', 'rpivotTable', 'futile.logger', 'zip', 'tidyr', 'leaflet.minicharts', 'dygraphs', 'xts', 'future'), repos='http://cran.us.r-project.org/')"


# environment variable for GDX API
USER miro
WORKDIR $APP
ENV R_GAMS_SYSDIR="/home/miro/gams"
COPY resources/docker /home/miro/gams

# copy MIRO to the image
COPY app.R global.R LICENSE /home/miro/app/
COPY conf /home/miro/app/conf
COPY JS /home/miro/app/JS
COPY modules /home/miro/app/modules
COPY R /home/miro/app/R
COPY tools/paver /home/miro/app/tools/paver
COPY UI /home/miro/app/UI
COPY www /home/miro/app/www

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/miro/app', port = 3838, host = '0.0.0.0')"]

