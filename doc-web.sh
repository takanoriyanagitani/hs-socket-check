#!/bin/bash

pkg=hs-socket-check

bname=$( cabal exec -- which "${pkg}" )
pname=$( dirname "${bname}" )
dname="${pname}/../../../../doc/html/${pkg}"


port=11380
addr=127.0.0.1

miniserve \
    --port ${port} \
    --interfaces "${addr}" \
    "${dname}"
