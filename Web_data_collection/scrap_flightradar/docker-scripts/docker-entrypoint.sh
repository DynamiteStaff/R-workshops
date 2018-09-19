#!/bin/bash

## Start up cron
service cron start

## Execute the supplied code
exec "$@"