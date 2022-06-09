FROM miro-admin as base

FROM base as builder

USER root

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    python3-pip

COPY requirements.txt /requirements.txt
RUN pip install --no-cache-dir --upgrade --user -r /requirements.txt


FROM base

LABEL com.gamsmiroauth.vendor="GAMS Development Corp."\
    com.gamsmiroauth.version="2.3.999"\
    com.gamsmiroauth.license="GPL-3.0"\
    com.gamsmiroauth.description="GAMS MIRO Docker image for auth container"\
    maintainer="fproske@gams.com"

USER root

ENV APP /app/app

COPY --chown=1000:1000 --from=builder /root/.local /home/miro/.local
ENV PATH=/home/miro/.local/bin:$PATH

COPY --chown=1000:1000 ./start.sh /start.sh
RUN chmod +x /start.sh

COPY --chown=1000:1000 ./gunicorn_conf.py /gunicorn_conf.py

COPY --chown=1000:1000 ./app $APP

EXPOSE 1234

USER miro
WORKDIR /app
CMD ["/start.sh"]
