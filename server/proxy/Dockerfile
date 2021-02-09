FROM maven:3.6.3-jdk-8 as builder

WORKDIR /app

COPY containerproxy containerproxy
RUN cd containerproxy && mvn -U clean install -DskipTests

COPY shinyproxy shinyproxy
COPY proxy/build.sh .

RUN chmod +x build.sh

RUN ./build.sh

FROM openjdk:8-alpine

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
