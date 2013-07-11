#!/bin/bash
#

function chopin() {
    sbcl --noinform --eval "(ql:quickload :chopin-routing)" \
	--eval "(in-package :chopin-routing)" \
	--eval "(push 'kernel-table-cleanup sb-ext:*exit-hooks*)" \
	--eval "(start-server)" \
	--noprint \
	--disable-debugger
}

func=chopin
export -f chopin

chopin

#gnome-terminal --tab -e "bash -c \"eval $func;exec bash\"" \
#--tab --title "routes" -e "bash -c \"while true; do clear; route -n; sleep 2; done\""
