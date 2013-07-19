#!/bin/bash
#
# <test_number> is for stats file filtering. Not mandatory.

if [ $# -lt 2 ]; then
    echo "Usage: $0 <{chopin|olsr}> <interface> <test_number>"
    exit 1
fi

export iface=$2

function chopin() {
    sbcl --noinform --eval "(ql:quickload :chopin-routing)" \
	--eval "(in-package :chopin-routing)" \
	--eval "(push 'kernel-table-cleanup sb-ext:*exit-hooks*)" \
	--eval "(start-server)" \
	--noprint \
	--disable-debugger
}

function olsr() {
    olsrd -i $iface
}

if [ $1 == "chopin" ]; then
    func=chopin
    file=chopin.stats$3
    export -f chopin
else
    func=olsr
    file=olsr.stats$3
    export -f olsr
fi

eval $func
