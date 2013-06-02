#!/bin/sh -e

# Create a swank server for now. Actual daemonize process will require more hacking.

if [ $# -lt 1 ]; then
    echo "Usage: $0 <port>"
    exit 1
fi

sar -n DEV,EDEV,UDP,TCP 1 -o utils/stats >/dev/null 2>&1 & 

sbcl --noinform --eval "(require :swank)" \
     --eval "(swank:create-server :port (parse-integer \"$1\") :style :spawn :dont-close t)" \
     --eval "(ql:quickload :chopin-routing)" \
     --eval "(in-package :chopin-routing)" \
     --eval "(push 'kernel-table-cleanup sb-ext:*exit-hooks*)" \
     --eval "(start-server)" \
     --noprint \
     --disable-debugger

sudo killall sar
