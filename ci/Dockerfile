FROM miro-ui:latest

LABEL com.gamsmiroci.vendor="GAMS Development Corp."\
com.gamsmiroci.license="GPL-3.0"\
com.gamsmiroci.description="GAMS MIRO CI image"\
maintainer="fproske@gams.com"

ARG GAMS_MAJOR=33
ARG GAMS_MINOR=1
ARG GAMS_MAINT=0

USER root

RUN apt-get update && apt-get install -y --no-install-recommends \
      libxml2-dev wget curl gpg-agent

RUN wget https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_MAJOR}.${GAMS_MINOR}.${GAMS_MAINT}/linux/linux_x64_64_sfx.exe && \
    chmod a+x linux_x64_64_sfx.exe && \
    ./linux_x64_64_sfx.exe && \
    rm linux_x64_64_sfx.exe && \
    mv "gams${GAMS_MAJOR}.${GAMS_MINOR}_linux_x64_64_sfx" "/opt/gams"

RUN R -e "install.packages(c('testthat', 'shinytest', 'xml2', 'precommit', 'styler'));"

# need to install Rcpp, plogr, BH again as we removed header files in miroui image
RUN mkdir /home/miro/r
COPY r/library_src /home/miro/r/library_src
COPY ci/install_pkg.R /home/miro/r/install_pkg.R
RUN Rscript /home/miro/r/install_pkg.R
RUN rm -rf /home/miro/r

# install nodejs, yarn and eslint
RUN curl -sL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
RUN sudo apt-get install -y nodejs
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
RUN apt-get update && apt-get install -y yarn python3-pip git
RUN pip3 install pre-commit

RUN groupadd -g 1001 miroci && \
    useradd -r -u 1001 -g miroci miroci

RUN mkdir /home/miroci
RUN chown -R miroci /home/miroci

# install openssh and rsync for deploying documentation
RUN apt-get update && apt-get install -y openssh-client rsync

USER miroci

RUN R -e "library(shinytest);installDependencies()"

RUN mkdir /home/miroci/ci
WORKDIR /home/miroci/ci
COPY ci/run.sh /etc/profile.d/ci.sh

ENV GAMS_SYS_DIR=/opt/gams
