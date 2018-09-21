#!/bin/sh

set -e

addgroup -g $GID scrap && adduser -s /bin/sh -D -G scrap -u $UID scrap

if [ "$(whoami)" == "root" ]; then
    chown -R scrap:scrap /usr/local/src/flight-scrap/docker-scripts
    chown --dereference scrap "/proc/$$/fd/1" "/proc/$$/fd/2" || :
    exec su-exec scrap "$@"
fi
