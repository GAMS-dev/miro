FROM python:3.8

LABEL "com.gamsmiro.vendor"="GAMS Development Corp."\
    version="0.0.1"\
    description="GAMS MIRO Server-Authentication Module"\
    maintainer="fproske@gams.com"

ENV APP /app

RUN groupadd -g 999 auth && useradd -u 999 -g auth auth

COPY --chown=999:999 ./start.sh /start.sh
RUN chmod +x /start.sh

COPY --chown=999:999 ./gunicorn_conf.py /gunicorn_conf.py

COPY --chown=999:999 ./requirements.txt /requirements.txt

RUN pip install --no-cache-dir \
    -r /requirements.txt

COPY --chown=999:999 ./app $APP

RUN chown -R auth:auth $APP

ENV PYTHONPATH=$APP

EXPOSE 80

USER auth
WORKDIR $APP
CMD ["/start.sh"]
