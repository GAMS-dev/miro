FROM miro-ui

LABEL com.gamsmiroadmin.vendor="GAMS Development Corp."\
com.gamsmiroadmin.version="2.1.2"\
com.gamsmiroadmin.license="GPL-3.0"\
com.gamsmiroadmin.description="GAMS MIRO Docker image for admin panel"\
maintainer="fproske@gams.com"


# copy MIRO admin app to the image
COPY . /home/miro/admin/

WORKDIR /home/miro/admin

CMD ["R", "-e", "shiny::runApp('/home/miro/admin', port = 3838, host = '0.0.0.0')"]
