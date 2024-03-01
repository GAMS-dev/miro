#!/bin/bash

CP_PATH=`find /app/containerproxy/target -regex '.*[0-9]\.jar'`
echo $CP_PATH

CP_VERSION=`echo $CP_PATH  | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`
echo $CP_VERSION

mkdir local-maven-repo

mvn org.apache.maven.plugins:maven-install-plugin:2.5.2:install-file \
    -Dfile=$CP_PATH -DgroupId=eu.openanalytics -DartifactId=eu.openanalytics.containerproxy \
    -Dversion=$CP_VERSION -Dpackaging=jar -DlocalRepositoryPath=/app/local-maven-repo


pushd /app/shinyproxy > /dev/null || exit 1
    mvn -U clean install
popd > /dev/null

SP_PATH=`find /app/shinyproxy/target -regex '.*[0-9]-exec\.jar'`
echo $SP_PATH

mv $SP_PATH /app/shinyproxy/target/shinyproxy.jar
