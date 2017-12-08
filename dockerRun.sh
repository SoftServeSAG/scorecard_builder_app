#!/bin/bash
APP_NAME=$1
APP_PORT=$2

MEMORY="2g"
SWAP_MEMORY="1g"

if [ $(docker inspect -f '{{.State.Running}}' $APP_NAMR) = "true" ]; then
  docker kill "${APP_NAME}"
fi

docker run --rm -d --memory="${MEMORY}" --memory-swap="${SWAP_MEMORY}" --name "${APP_NAME}" -p "${APP_PORT}:3838" "${APP_NAME}"
