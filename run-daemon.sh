#!/bin/bash
#
# <test_number> is for stats file filtering. Not mandatory.

if [ $# -lt 2 ]; then
    echo "Usage: $0 <{chopin|olsr}> <interface> <test_number>"
    exit 1
fi

function chopin() {
    sbcl --noinform --eval "(ql:quickload :chopin-routing)" \
	--eval "(in-package :chopin-routing)" \
	--eval "(push 'kernel-table-cleanup sb-ext:*exit-hooks*)" \
	--eval "(start-server)" \
	--noprint \
	--disable-debugger
}

if [ $1 == "chopin" ]; then
    func=chopin
    file=chopin.stats$3
else
    func="olsrd -i $2"
    file=olsr.stats$3
fi

sar -n DEV,EDEV,UDP,TCP,ETCP 1 -o utils/$file >/dev/null 2>&1 & 
${func}
sudo killall sar
