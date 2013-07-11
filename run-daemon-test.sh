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

DATE=$(date +"%Y%m%d%H%M%S")
HOST=$(hostname)
FILENAME=$file.$HOST.$DATE
gnome-terminal --tab -e "bash -c \"eval $func;exec bash\"" --tab --title "sar" -e "bash -c \"sar -n DEV,EDEV,UDP,TCP,ETCP 1 -o utils/$FILENAME;exec bash\"" \
--tab --title "routes" -e "bash -c \"while true; do clear; route -n; sleep 2; done\""

echo "Copying stats to Dropbox ..."
cp utils/"$FILENAME" ~/Dropbox/TraxBot/chopin-routing-josepereira/
