#!/bin/sh -e

# Create a swank server for now. Actual daemonize process will require more hacking.

if [ $# -lt 1 ]; then
    echo "Usage: $0 <port>"
    exit 1
fi

sbcl --noinform --eval "(require :swank)" \
     --eval "(swank:create-server :port (parse-integer \"$1\") :style :spawn :dont-close t)" \
     --eval "(ql:quickload :chopin-routing)" \
     --eval "(in-package :chopin-routing)"
