FROM miro-ui:latest AS miro-pkg-builder

USER root

ARG R_BASE_VERSION=4.2.1
ARG NODE_MAJOR=20

RUN apt-get update && apt-get install -y \
    sudo \
    wget \
    libxml2-dev \
    libssl-dev \
    r-base-dev=${R_BASE_VERSION}*

RUN R -e "install.packages(c('testthat', 'shinytest2', 'xml2', 'precommit', 'docopt', 'styler', 'oysteR'));"

COPY r/library_src /home/miro/r/library_src
COPY ci/install_pkg.R /home/miro/r/install_pkg.R
RUN Rscript /home/miro/r/install_pkg.R



FROM miro-ui:latest

LABEL com.gamsmiroci.vendor="GAMS Development Corp."\
    com.gamsmiroci.license="GPL-3.0"\
    com.gamsmiroci.description="GAMS MIRO CI image"\
    maintainer="fproske@gams.com"

ARG GAMS_MAJOR=33
ARG GAMS_MINOR=1
ARG GAMS_MAINT=0

ARG NODE_MAJOR=20

USER root

RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev wget curl gpg-agent

RUN wget https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_MAJOR}.${GAMS_MINOR}.${GAMS_MAINT}/linux/linux_x64_64_sfx.exe && \
    chmod a+x linux_x64_64_sfx.exe && \
    ./linux_x64_64_sfx.exe && \
    rm linux_x64_64_sfx.exe && \
    mv "gams${GAMS_MAJOR}.${GAMS_MINOR}_linux_x64_64_sfx" "/opt/gams"

COPY --from=miro-pkg-builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# install nodejs, yarn and eslint
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list
RUN apt-get update && apt-get install -y nodejs
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt-get update && apt-get install -y yarn python3-pip python3-venv git
RUN pip3 install pre-commit pip-audit pandas gamspy

# install openssh and rsync for deploying documentation
RUN apt-get update && apt-get install -y openssh-client rsync

# install chrome for UI tests
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
    apt install -y ./google-chrome-stable_current_amd64.deb && \
    rm ./google-chrome-stable_current_amd64.deb

COPY ci/run.sh /etc/profile.d/ci.sh

ENV GAMS_SYS_DIR=/opt/gams
