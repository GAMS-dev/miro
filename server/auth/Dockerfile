FROM miro-admin

LABEL com.gamsmiauth.vendor="GAMS Development Corp."\
    com.gamsmiauth.version="1.3.99"\
    com.gamsmiauth.license="GPL-3.0"\
    com.gamsmiauth.description="GAMS MIRO Docker image for auth container"\
    maintainer="fproske@gams.com"

USER root

ENV APP /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    python3-pip

COPY --chown=1000:1000 ./start.sh /start.sh
RUN chmod +x /start.sh

COPY --chown=1000:1000 ./gunicorn_conf.py /gunicorn_conf.py

COPY --chown=1000:1000 ./requirements.txt /requirements.txt

RUN pip3 install --no-cache-dir \
    -r /requirements.txt

COPY --chown=1000:1000 ./app $APP

RUN chown -R miro:miro $APP

ENV PYTHONPATH=$APP

EXPOSE 1234

USER miro
WORKDIR $APP
CMD ["/start.sh"]
