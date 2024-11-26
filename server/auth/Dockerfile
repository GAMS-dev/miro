FROM --platform=linux/amd64 miro-admin as base

FROM --platform=linux/amd64 base as builder

USER root

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    python3-pip python3-venv

COPY requirements.txt /requirements.txt
RUN python3 -m venv /py-env \
	&& /py-env/bin/pip install -U -r /requirements.txt


FROM --platform=linux/amd64 base

LABEL com.gamsmiroauth.vendor="GAMS Development Corp."\
    com.gamsmiroauth.version="2.10.9999"\
    com.gamsmiroauth.license="GPL-3.0"\
    com.gamsmiroauth.description="GAMS MIRO Docker image for auth container"\
    maintainer="fproske@gams.com"

USER root

ENV APP /app/app

COPY --chown=1001:1001 --from=builder /py-env /py-env
ENV PATH=/py-env/bin:$PATH

COPY --chown=1001:1001 ./start.sh /start.sh
RUN chmod +x /start.sh

COPY --chown=1001:1001 ./gunicorn_conf.py /gunicorn_conf.py

COPY --chown=1001:1001 ./app $APP

EXPOSE 1234

USER miro
WORKDIR /app
CMD ["/start.sh"]
