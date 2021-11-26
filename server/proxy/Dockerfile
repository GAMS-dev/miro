FROM maven:3.8.4-jdk-11 as builder

WORKDIR /app

COPY containerproxy/pom.xml containerproxy/pom.xml
RUN cd containerproxy && mvn -U verify clean --fail-never

COPY containerproxy containerproxy
RUN cd containerproxy && mvn -U install -DskipTests

COPY shinyproxy shinyproxy
COPY proxy/build.sh .

RUN chmod +x build.sh

RUN ./build.sh

FROM alpine:3.15

RUN apk --no-cache add openjdk11-jre

RUN addgroup -g 1000 -S miro && \
    adduser -u 1000 -S miro -G miro

WORKDIR /home/miroproxy/


COPY --from=builder /app/shinyproxy/target/shinyproxy.jar shinyproxy.jar
COPY proxy/favicon.ico proxy/boot.sh ./
RUN chmod +x boot.sh
COPY proxy/templates templates

RUN chown -R miro:miro ./
USER miro

ENTRYPOINT ["./boot.sh"]
