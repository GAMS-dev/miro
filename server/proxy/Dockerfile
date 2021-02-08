FROM openjdk:8-alpine

RUN addgroup -g 1000 -S miro && \
    adduser -u 1000 -S miro -G miro

WORKDIR /home/miroproxy/


COPY shinyproxy-2.4.3.jar shinyproxy.jar
COPY favicon.ico boot.sh ./
RUN chmod +x boot.sh
COPY templates templates

RUN chown -R miro:miro ./
USER miro

ENTRYPOINT ["./boot.sh"]
