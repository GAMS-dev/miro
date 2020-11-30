FROM gamsmiro-ui

LABEL com.gamsmiroci.vendor="GAMS Development Corp."\
com.gamsmiroci.license="GPL-3.0"\
com.gamsmiroci.description="GAMS MIRO CI image"\
maintainer="fproske@gams.com"

ARG GAMS_MAJOR=33
ARG GAMS_MINOR=1
ARG GAMS_MAINT=0

USER root

RUN apt-get install -y --no-install-recommends \
      libxml2-dev wget

RUN wget https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_MAJOR}.${GAMS_MINOR}.${GAMS_MAINT}/linux/linux_x64_64_sfx.exe && \
    chmod a+x linux_x64_64_sfx.exe && \
    ./linux_x64_64_sfx.exe && \
    rm linux_x64_64_sfx.exe && \
    mv "gams${GAMS_MAJOR}.${GAMS_MINOR}_linux_x64_64_sfx" "/opt/gams"

RUN R -e "install.packages(c('testthat', 'shinytest', 'xml2'));"

# need to install Rcpp, plogr, BH again as we removed header files in miroui image
RUN R -e "install.packages(c('Rcpp', 'plogr', 'BH', 'RSQLite'))"

# install nodejs, yarn and eslint
RUN apt-get update && apt-get install -y curl
RUN curl -sL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
RUN apt-get install -y nodejs
RUN wget --quiet -O - /tmp/pubkey.gpg https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
RUN apt-get update && apt-get install -y yarn
RUN yarn global add eslint

RUN groupadd -g 1001 miroci && \
    useradd -r -u 1001 -g miroci miroci

RUN mkdir /home/miroci
RUN chown -R miroci /home/miroci

USER miroci

RUN R -e "library(shinytest);installDependencies()"

RUN mkdir /home/miroci/ci
WORKDIR /home/miroci/ci
COPY run.sh /etc/profile.d/ci.sh

ENV GAMS_SYS_DIR=/opt/gams
