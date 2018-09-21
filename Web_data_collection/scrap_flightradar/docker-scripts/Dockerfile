FROM artemklevtsov/r-alpine:latest as baseImage

ENV TZ=UTC

ENTRYPOINT [ "/usr/local/src/flight-scrap/docker-scripts/entrypoint.sh" ]

CMD ["supercronic", "crontab"]
WORKDIR /usr/local/src/flight-scrap/docker-scripts

COPY Makevars /root/.R/Makevars
RUN mkdir -p /usr/local/src/flight-scrap/ccache

RUN echo http://nl.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories
RUN apk --no-cache add busybox-suid curl ccache su-exec

COPY install_packages.r .
RUN Rscript install_packages.r

COPY . /usr/local/src/flight-scrap/docker-scripts

RUN mkdir -p /usr/local/src/flight-scrap/docker-scripts/data

ENV SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.1.5/supercronic-linux-amd64 \
    SUPERCRONIC=supercronic-linux-amd64 \
    SUPERCRONIC_SHA1SUM=9aeb41e00cc7b71d30d33c57a2333f2c2581a201

RUN curl -fsSLO "$SUPERCRONIC_URL" \
 && echo "${SUPERCRONIC_SHA1SUM}  ${SUPERCRONIC}" | sha1sum -c - \
 && chmod +x "$SUPERCRONIC" \
 && mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}" \
 && ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic


