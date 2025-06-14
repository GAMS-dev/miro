FROM --platform=linux/amd64 maven:3.9.6-eclipse-temurin-17 AS builder

WORKDIR /app

COPY containerproxy/pom.xml containerproxy/pom.xml
RUN cd containerproxy && mvn -U verify clean --fail-never

COPY containerproxy containerproxy
RUN cd containerproxy && mvn -U install -DskipTests

COPY shinyproxy shinyproxy
COPY proxy/build.sh .

RUN chmod +x build.sh

RUN ./build.sh

FROM --platform=linux/amd64 alpine:3.19

RUN apk update && \
    apk upgrade --available && \
    apk --no-cache add openjdk17-jre

RUN addgroup -g 1001 -S miro && \
    addgroup -g 1000 -S miro2 && \
    adduser -u 1001 -S miro -G miro && \
    addgroup miro root && \
    addgroup miro miro2

WORKDIR /home/miroproxy/


COPY --from=builder /app/shinyproxy/target/shinyproxy.jar shinyproxy.jar
COPY proxy/favicon.ico proxy/boot.sh ./
RUN chmod +x boot.sh
COPY proxy/templates templates

RUN chown -R miro:miro ./
USER miro

ENTRYPOINT ["./boot.sh"]
