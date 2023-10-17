FROM python:3-alpine

RUN apk update && \
    apk upgrade --available

RUN adduser -D -u 1000 mirodoc

WORKDIR /home/mirodoc

COPY requirements.txt requirements.txt
RUN python -m venv venv
RUN venv/bin/pip install -r requirements.txt
RUN venv/bin/pip install gunicorn

COPY app app
COPY mirodoc.py boot.sh ./
RUN chmod +x boot.sh

ENV FLASK_APP mirodoc.py

RUN chown -R mirodoc:mirodoc ./
USER mirodoc

EXPOSE 5000
ENTRYPOINT ["./boot.sh"]
