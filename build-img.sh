#!/bin/bash

tag=hs-socket-check:0.1.0.0

build_apple(){
    container image inspect "${tag}" |
        jq --raw-output '.[].name' |
        fgrep -q "$tag" \
        && return

    echo building image "${tag}"...

    container \
        build \
        --file ./Dockerfile \
        --platform linux/arm64 \
        --progress plain \
        --tag "${tag}" \
        .
}

build_apple
